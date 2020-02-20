package org.jetbrains.plugins.scala.findUsages.compilerReferences
package compilation

import java.util.UUID

import com.intellij.compiler.impl.ExitStatus
import com.intellij.compiler.server.{BuildManagerListener, CustomBuilderMessageHandler}
import com.intellij.openapi.compiler.{CompilationStatusListener, CompileContext, CompilerTopics}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.compilerReferences.{Builder, Messages}
import org.jetbrains.plugins.scala.findUsages.compilerReferences.ScalaCompilerReferenceService.CompilerIndicesState
import org.jetbrains.plugins.scala.indices.protocol.jps.JpsCompilationInfo
import org.jetbrains.plugins.scala.project.ModuleExt

private[compilerReferences] class JpsCompilationWatcher(
  override val project:          Project,
  override val transactionGuard: TransactionGuard[CompilerIndicesState]
) extends CompilationWatcher[CompilerMode.JPS.type] { self =>

  /**
    * Designed to workaround the up-to-date check, which triggers build started, but not any of the
    * builders/compilations finished.
    * Increment each time build is started, decrement each time builder reports indexing start,
    * if build finishes and the value is greate than 0, invoke onCompilationFinish manually,
    * to account for an up-to-date check.
    */
  @volatile var buildCompilationDiff = 0

  override def compilerMode: CompilerMode.JPS.type = CompilerMode.JPS

  private[this] def handleBuilderMessage(
    messageType: String,
    messageText: String,
    publisher:   CompilerIndicesEventPublisher
  ): Unit =
    messageType match {
      case Messages.compilationDataType =>
        val buildData = Messages.decompressCompilationInfo(messageText)

        buildData.fold(
          error => {
            publisher.onError(ScalaBundle.message("malformed.message.from.builder", messageText), Option(error))
          },
          publisher.processCompilationInfo(_, offline = false)
        )
      case Messages.compilationStartedType =>
        val isCleanBuild = java.lang.Boolean.valueOf(messageText)
        buildCompilationDiff -= 1
        publisher.startIndexing(isCleanBuild)
      case Messages.compilationFinishedType => publisher.finishIndexing()
      case _                               => ()
    }

  override def start(): Unit = {
    val connection = project.getMessageBus.connect(project)

    connection.subscribe(
      CustomBuilderMessageHandler.TOPIC,
      new CustomBuilderMessageHandler {
        override def messageReceived(
          builderId:   String,
          messageType: String,
          messageText: String
        ): Unit =
          if (builderId == Builder.id) processEventInTransaction(handleBuilderMessage(messageType, messageText, _))
      }
    )

    /* HACK */
    /* in case of a compilation of an up-to-date module
    *  we do not receive any builder events (since no builders are actually executed)
    *  but we still have to mark the module as clean in DirtyScopeHolder afterwards */
    connection.subscribe(CompilerTopics.COMPILATION_STATUS, new CompilationStatusListener {
      override def compilationFinished(
        aborted:        Boolean,
        errors:         Int,
        warnings:       Int,
        compileContext: CompileContext
      ): Unit = {
        val timestamp   = System.currentTimeMillis()

        // noinspection ScalaDeprecation
        // this key is declared private in CompileDriver
        val key         = Option(Key.findKeyByName("COMPILE_SERVER_BUILD_STATUS"))
        val status      = key.flatMap(k => Option(compileContext.getUserData(k)))
        val wasUpToDate = status.contains(ExitStatus.UP_TO_DATE)

        val modules =
          Option(compileContext.getCompileScope)
            .fold(Set.empty[String])(
              _.getAffectedModules.filter(_.hasScala).map(_.getName).toSet
            )

        // @TODO: handle the following scenario:
        // @TODO: no indices are present and all modules are up-to-date
        // @TODO: currently it will simply do nothing
        processEventInTransaction { publisher =>
          if (wasUpToDate) {
            publisher.startIndexing(false)
            buildCompilationDiff -= 1
            val info = JpsCompilationInfo(modules, Set.empty, Set.empty, timestamp)
            publisher.processCompilationInfo(info, offline = false)
            publisher.onCompilationFinish(success = true)
          } else publisher.onCompilationFinish(success = !aborted && errors == 0)
        }
      }
    })
    /* HACK */

    connection.subscribe(BuildManagerListener.TOPIC, new BuildManagerListener {
      override def buildStarted(
        project:    Project,
        sessionId:  UUID,
        isAutomake: Boolean
      ): Unit = if (project == self.project) {
        processEventInTransaction { publisher =>
          buildCompilationDiff += 1
          publisher.onCompilationStart()
        }
      }

      /**
        * Important, because compilerManager.isUpToDate(scope)
        * will trigger buildStarted, but not builders/compilation finished.
        */
      override def buildFinished(
        project:    Project,
        sessionId:  UUID,
        isAutomake: Boolean
      ): Unit = if (project == self.project)
        processEventInTransaction { publisher =>
          if (buildCompilationDiff > 0) {
            buildCompilationDiff -= 1
            publisher.onCompilationFinish(true)
          }
        }
    })
  }
}
