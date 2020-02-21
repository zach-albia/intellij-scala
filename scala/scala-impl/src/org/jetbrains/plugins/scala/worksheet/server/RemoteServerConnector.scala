package org.jetbrains.plugins.scala
package worksheet.server

import java.io._
import java.nio.charset.StandardCharsets
import java.util.Base64

import com.intellij.compiler.CompilerMessageImpl
import com.intellij.openapi.compiler.{CompilerMessage, CompilerMessageCategory, CompilerPaths}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiManager
import org.jetbrains.jps.incremental.ModuleLevelBuilder.ExitCode
import org.jetbrains.jps.incremental.messages.BuildMessage
import org.jetbrains.jps.incremental.messages.BuildMessage.Kind
import org.jetbrains.jps.incremental.scala.{Client, DummyClient}
import org.jetbrains.plugins.scala.compiler.data.worksheet.{WorksheetArgs, WorksheetArgsPlain, WorksheetArgsRepl}
import org.jetbrains.plugins.scala.compiler.{NonServerRunner, PluginJars, RemoteServerConnectorBase, RemoteServerRunner}
import org.jetbrains.plugins.scala.lang.psi.api.{ScFile, ScalaFile}
import org.jetbrains.plugins.scala.project.ModuleExt
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerSettings
import org.jetbrains.plugins.scala.worksheet.actions.WorksheetFileHook
import org.jetbrains.plugins.scala.worksheet.processor.WorksheetDefaultSourcePreprocessor
import org.jetbrains.plugins.scala.worksheet.runconfiguration.ReplModeArgs
import org.jetbrains.plugins.scala.worksheet.server.RemoteServerConnector._
import org.jetbrains.plugins.scala.worksheet.settings.WorksheetFileSettings
import org.jetbrains.plugins.scala.worksheet.ui.printers.WorksheetEditorPrinterRepl

// TODO: split to REPL and PLAIN args, change serialization format
private[worksheet]
class RemoteServerConnector(
  module: Module,
  worksheetPsiFile: ScFile,
  worksheet: File,
  output: File,
  worksheetClassName: String,
  replArgs: Option[ReplModeArgs],
  makeType: WorksheetMakeType,
  needsCheck: Boolean
) extends RemoteServerConnectorBase(module, Seq(worksheet), output, needsCheck) {

  override protected def compilerSettings: ScalaCompilerSettings =
    WorksheetFileSettings(worksheetPsiFile).getCompilerProfile.getSettings

  override val worksheetArgs: Option[WorksheetArgs] =
    makeType match {
      case OutOfProcessServer => None
      case _ =>
        val args = replArgs match {
          case Some(ra) =>
            WorksheetArgsRepl(
              sessionId = ra.path,
              ra.codeChunk,
              outputDirs
            )
          case None     =>
            WorksheetArgsPlain(
              worksheetClassName,
              PluginJars.runnersJar,
              worksheet,
              worksheet.getName,
              output +: outputDirs,
            )
        }
        Some(args)
    }

  override val scalaParameters: Seq[String] = {
    val options = super.scalaParameters
    if (module.hasScala3) {
      val extraOptions = Seq(
        "-color:never", // by default dotty prints lots of color, can't handle for now
        "-noindent", "-old-syntax" // do avoid "Line is indented too far to the left" warnings
      )
      options.filterNot(_.startsWith("-g:")) ++ extraOptions
    } else {
      options
    }
  }

  // TODO: make something more advanced than just `callback: Runnable`: error reporting, Future, Task, etc...
  // TODO: add logging across all these callbacks in RunWorksheetAction, WorksheetCompiler, RemoteServerConnector...
  // NOTE: for now this method is non-blocking for runType == NonServer and blocking for other run types
  def compileAndRun(originalFile: VirtualFile,
                    consumer: RemoteServerConnector.CompilerInterface,
                    callback: RemoteServerConnectorResult => Unit): Unit = {

    val project = module.getProject

    val client = new MyTranslatingClient(project, originalFile, consumer)

    val process = {
      val worksheetProcess = makeType match {
        case InProcessServer | OutOfProcessServer =>
          val runner = new RemoteServerRunner(project)
          val argumentsFinal = arguments
          runner.buildProcess(argumentsFinal, client)

        case NonServer =>
          val argumentsFinal = NoToken +: arguments
          val argumentsEncoded = argumentsFinal.map { arg =>
            val argFixed = if(arg.isEmpty) "#STUB#" else arg
            Base64.getEncoder.encodeToString(argFixed.getBytes(StandardCharsets.UTF_8))
          }
          val runner = new NonServerRunner(project)
          runner.buildProcess(argumentsEncoded, client)
      }

      if (worksheetProcess == null) {
        callback(RemoteServerConnectorResult.ProcessTerminatedError(ExitCode.ABORT))
        return
      }

      val psiFile = inReadAction {
        PsiManager.getInstance(project).findFile(originalFile)
      }
      val fileToReHighlight = psiFile match {
        case scalaFile: ScalaFile if WorksheetFileSettings.isRepl(scalaFile) => Some(scalaFile)
        case _ => None
      }

      WorksheetFileHook.updateStoppableProcess(originalFile, Some(() => worksheetProcess.stop()))
      worksheetProcess.addTerminationCallback { exception =>
        WorksheetFileHook.updateStoppableProcess(originalFile, None)

        fileToReHighlight.foreach(WorksheetEditorPrinterRepl.rehighlight)

        val result = exception match {
          case Some(_) =>
            RemoteServerConnectorResult.ProcessTerminatedError(ExitCode.ABORT)
          case _ if consumer.isCompiledWithErrors =>
            RemoteServerConnectorResult.CompilationError
          case _ =>
            makeType match {
              case OutOfProcessServer => RemoteServerConnectorResult.Compiled(worksheetClassName, output)
              case _                  => RemoteServerConnectorResult.CompiledAndEvaluated
            }
        }

        callback.apply(result)
      }

      worksheetProcess
    }

    // exceptions thrown inside the process should be propagated to callback via termination callback
    process.run()
  }

  private def outputDirs: Seq[File] = {
    val modules = ModuleRootManager.getInstance(module).getDependencies :+ module
    val strings = modules.map(CompilerPaths.getModuleOutputPath(_, false))
    strings.map(new File(_))
  }
}

private[worksheet]
object RemoteServerConnector {

  sealed trait RemoteServerConnectorResult
  object RemoteServerConnectorResult {
    case class Compiled(worksheetClassName: String, outputDir: File) extends RemoteServerConnectorResult
    case object CompiledAndEvaluated extends RemoteServerConnectorResult

    sealed trait Error extends RemoteServerConnectorResult
    object CompilationError extends Error // assuming that errors are collected by CompilerInterface
    sealed trait UnhandledError extends Error
    final case class ProcessTerminatedError(rc: ExitCode) extends UnhandledError
    final case class ExpectedError(cause: Throwable) extends UnhandledError
    final case class UnexpectedError(cause: Throwable) extends UnhandledError
  }

  private class MyTranslatingClient(project: Project, worksheet: VirtualFile, consumer: CompilerInterface) extends DummyClient {
    private val endMarker = WorksheetDefaultSourcePreprocessor.END_GENERATED_MARKER

    override def progress(text: String, done: Option[Float]): Unit =
      consumer.progress(text, done)

    override def trace(exception: Throwable): Unit =
      consumer.trace(exception)

    override def message(msg: Client.ClientMsg): Unit = {
      val Client.ClientMsg(kind, text, source, line, column) = msg
      val lines = (if(text == null) "" else "").split("\n")
      val linesLength = lines.length

      val differ = if (linesLength > 2) {
        val endLineIdx = lines(linesLength - 2).indexOf(endMarker)
        if (endLineIdx != -1) {
          endLineIdx + endMarker.length
        } else 0
      } else 0

      val finalText = if (differ == 0) text else {
        val buffer = new StringBuilder

        for (j <- 0 until (linesLength - 2)) {
          buffer.append(lines(j)).append("\n")
        }

        val lines1 = lines(linesLength - 1)

        buffer
          .append(lines(linesLength - 2).substring(differ)).append("\n")
          .append(if (lines1.length > differ) lines1.substring(differ) else lines1).append("\n")

        buffer.toString()
      }

      // TODO: current line & column calculation are broken
      val line1 = line.map(i => i - 4).map(_.toInt).getOrElse(-1)
      val column1 = column.map(_ - differ).map(_.toInt).getOrElse(-1)

      val category = toCompilerMessageCategory(kind)

      val message = new CompilerMessageImpl(project, category, finalText, worksheet, line1, column1, null)
      consumer.message(message)
    }

    private def toCompilerMessageCategory(kind: Kind): CompilerMessageCategory = {
      import BuildMessage.Kind._
      kind match {
        case INFO | JPS_INFO | OTHER        => CompilerMessageCategory.INFORMATION
        case ERROR | INTERNAL_BUILDER_ERROR => CompilerMessageCategory.ERROR
        case PROGRESS                       => CompilerMessageCategory.STATISTICS
        case WARNING                        => CompilerMessageCategory.WARNING
      }
    }

    override def worksheetOutput(text: String): Unit =
      consumer.worksheetOutput(text)
  }

  // Worksheet Integration Tests rely on that this is the main entry point for all compiler messages
  trait CompilerMessagesConsumer {
    def message(message: CompilerMessage): Unit
  }

  trait CompilerInterface extends CompilerMessagesConsumer {
    def progress(text: String, done: Option[Float]): Unit

    def worksheetOutput(text: String): Unit
    def trace(thr: Throwable): Unit

    def isCompiledWithErrors: Boolean
  }
}
