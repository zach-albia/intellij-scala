package org.jetbrains.plugins.scala.findUsages.compilerReferences
package search

import java.awt.{List => _}
import java.util.concurrent.locks.ReentrantLock

import com.intellij.find.FindManager
import com.intellij.find.impl.FindManagerImpl
import com.intellij.openapi.application.TransactionGuard
import com.intellij.openapi.module.Module
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.ui.{DialogWrapper, Messages}
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.task.ProjectTaskManager
import com.intellij.util.Processor
import com.intellij.util.containers.ContainerUtil
import com.intellij.util.messages.MessageBusConnection
import javax.swing._
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.SearchTargetExtractors.{SAMType, UsageType}
import org.jetbrains.plugins.scala.findUsages.compilerReferences.compilation.CompilerMode
import org.jetbrains.plugins.scala.findUsages.compilerReferences.indices.ScalaCompilerIndices
import org.jetbrains.plugins.scala.findUsages.compilerReferences.search.ImplicitUsagesSearchDialogs._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.search.UsageToPsiElements._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.settings.CompilerIndicesSettings
import org.jetbrains.plugins.scala.findUsages.factory.{CompilerIndicesFindUsagesHandler, ScalaFindUsagesHandler, ScalaFindUsagesHandlerFactory}
import org.jetbrains.plugins.scala.project._
import org.jetbrains.plugins.scala.util.ImplicitUtil._
import org.jetbrains.sbt.shell.SbtShellCommunication

import scala.collection.JavaConverters._

private class CompilerIndicesReferencesSearcher
  extends CompilerIndicesSearcher[
    PsiNamedElement,
    PsiReference,
    CompilerIndicesReferencesSearch.SearchParameters
  ](true) {

  override def processQuery(
    parameters: CompilerIndicesReferencesSearch.SearchParameters,
    consumer:   Processor[_ >: PsiReference]
  ): Unit = {
    val target  = parameters.element
    val project = target.getProject
    val service = ScalaCompilerReferenceService(project)
    val usages  = service.usagesOf(target)
    processResultsFromCompilerService(target, usages, project, consumer)
  }

    override protected def processMatchingElements(
      target:             PsiNamedElement,
      usage:              UsagesInFile,
      isPossiblyOutDated: Boolean,
      candidates:         ElementsInContext,
      processor:          Processor[_ >: PsiReference]
    ): Boolean = {
      val ElementsInContext(elements, file, doc) = candidates

      val refsWithLine = elements.flatMap { case (e, line) =>
        val maybeRef = target.refOrImplicitRefIn(e)
        maybeRef.foreach(ref => processor.process(ref))
        maybeRef.map(_ -> line)
      }.toList

      val extraLines = usage.lines.diff(refsWithLine.map(_._2))

      extraLines.foldLeft(true) { case (isFullyProcessed, line) =>
        val offset = doc.getLineStartOffset(line - 1)
        if (!isPossiblyOutDated) {
          val ref = UnresolvedImplicitReference(target, file, offset)
          processor.process(ref)
          isFullyProcessed
        } else false
      }
    }
}

object CompilerIndicesReferencesSearcher {
  private[this] var pendingConnection: MessageBusConnection = _

  private[findUsages] sealed trait BeforeIndicesSearchAction {
    def runAction(): Boolean
  }

  private[findUsages] case object CancelSearch extends BeforeIndicesSearchAction {
    override def runAction(): Boolean = false
  }

  /**
    * This is a little more optimal than doing a full project rebuild,
    * basically it just builds the project but uses underlying (sbt or jps specific)
    * machinery to extract not just incremental, but full source <-> class mappings.
    */
  private[findUsages] final case class RebuildIndices(project: Project, target: PsiElement)
      extends BeforeIndicesSearchAction {

    override def runAction(): Boolean = {
      val modules = project.modules

      ScalaCompilerReferenceService(project).inTransaction {
        case (CompilerMode.JPS, _) =>
          val manager = ProjectTaskManager.getInstance(project)
          runSearchAfterIndexingFinishedAsync(modules, project, target)
          manager.build(modules.toArray: _*)
        case (CompilerMode.SBT, _) =>
          runSearchAfterIndexingFinishedAsync(modules, project, target)
          val shell = SbtShellCommunication.forProject(project)
          shell.command("rebuildIdeaIndices")
      }

      false
    }
  }

  private[findUsages] final case class BuildModules(
    target:  PsiElement,
    project: Project,
    modules: Set[Module]
  ) extends BeforeIndicesSearchAction {
    override def runAction(): Boolean =
      if (modules.nonEmpty) {
        val manager = ProjectTaskManager.getInstance(project)
        runSearchAfterIndexingFinishedAsync(modules, project, target)
        manager.build(modules.toArray: _*)
        false
      } else true
  }

  private[this] val lock                      = new ReentrantLock()
  private[this] val indexingFinishedCondition = lock.newCondition()

  private[this] def showProgressIndicator(project: Project): Unit = {
    val awaitIndexing = task(project, ScalaBundle.message("bytecode.indices.progress.title")) { _ =>
      lock.locked(indexingFinishedCondition.awaitUninterruptibly())
    }
    ProgressManager.getInstance().run(awaitIndexing)
  }

  private[this] def runSearchAfterIndexingFinishedAsync(
    targetModules: Iterable[Module],
    project:       Project,
    target:        PsiElement
  ): Unit = {
    if (pendingConnection ne null) {
      pendingConnection.disconnect()
      lock.locked(indexingFinishedCondition.signal())
    }

    pendingConnection = project.getMessageBus.connect(project)

    pendingConnection.subscribe(CompilerReferenceServiceStatusListener.topic, new CompilerReferenceServiceStatusListener {
      private[this] val targetModuleNames = ContainerUtil.newConcurrentSet[String]

      targetModuleNames.addAll(targetModules.collect {
        case module if module.isSourceModule => module.getName
      }.asJavaCollection)

      override def onIndexingPhaseStarted(): Unit = showProgressIndicator(project)

      override def onCompilationInfoIndexed(modules: Set[String]): Unit =
        targetModuleNames.removeAll(modules.asJava)

      override def onIndexingPhaseFinished(success: Boolean): Unit = {
        if (success) {
          if (targetModuleNames.isEmpty) {
            lock.locked(indexingFinishedCondition.signal())
            val findManager = FindManager.getInstance(project).asInstanceOf[FindManagerImpl]
            val factory     = ScalaFindUsagesHandlerFactory.getInstance(project)

            val handler = inReadAction(target match {
              case SAMType(_) => new ScalaFindUsagesHandler(target, factory)
              case _          => new CompilerIndicesFindUsagesHandler(target, factory)
            })

            val runnable: Runnable = () =>
              findManager.getFindUsagesManager.findUsages(
                handler.getPrimaryElements,
                handler.getSecondaryElements,
                handler,
                handler.getFindUsagesOptions(),
                false
              )

            pendingConnection.disconnect()
            TransactionGuard.getInstance().submitTransactionAndWait(runnable)
          }
        } else {
          lock.locked(indexingFinishedCondition.signal())
          pendingConnection.disconnect()
        }
      }
    })
  }

  private[findUsages] def assertSearchScopeIsSufficient(
    target:    PsiNamedElement,
    usageType: UsageType
  ): Option[BeforeIndicesSearchAction] = {
    val project = target.getProject
    val service = ScalaCompilerReferenceService(project)

    if (!CompilerIndicesSettings(project).isIndexingEnabled) {
      inEventDispatchThread(new EnableCompilerIndicesDialog(project, canBeParent = false, usageType).show())
      Option(CancelSearch)
    } else if (service.isIndexingInProgress) {
      inEventDispatchThread(showIndexingInProgressDialog(project))
      Option(CancelSearch)
    } else {
      val (dirtyModules, upToDateModules) = dirtyModulesInDependencyChain(target)
      val validIndexExists                = upToDateCompilerIndexExists(project, ScalaCompilerIndices.version)

      if (dirtyModules.nonEmpty || !validIndexExists) {
        var action: Option[BeforeIndicesSearchAction] = None

        val dialogAction =
          () =>
            action = showRebuildSuggestionDialog(
              project,
              dirtyModules,
              upToDateModules,
              validIndexExists,
              target,
              usageType
          )

        inEventDispatchThread(dialogAction())
        action
      } else None
    }
  }

  private[this] def inEventDispatchThread[T](body: => T): Unit =
    if (SwingUtilities.isEventDispatchThread) body
    else                                      invokeAndWait(body)

  private[this] def dirtyModulesInDependencyChain(element: PsiElement): (Set[Module], Set[Module]) = {
    val project          = element.getProject
    val file             = PsiTreeUtil.getContextOfType(element, classOf[PsiFile]).getVirtualFile
    val index            = ProjectFileIndex.getInstance(project)
    val modules          = index.getOrderEntriesForFile(file).asScala.map(_.getOwnerModule).toSet
    val dirtyScopeHolder = ScalaCompilerReferenceService(project).getDirtyScopeHolder
    val dirtyScopes      = dirtyScopeHolder.dirtyScope
    modules.partition(dirtyScopes.isSearchInModuleContent)
  }

  private[this] def showIndexingInProgressDialog(project: Project): Unit = {
    val message = ScalaBundle.message("bytecode.indices.unavailable")
    Messages.showInfoMessage(project, message, ScalaBundle.message("bytecode.indices.in.progress"))
  }

  private[this] def showRebuildSuggestionDialog(
    project:          Project,
    dirtyModules:     Set[Module],
    upToDateModules:  Set[Module],
    validIndexExists: Boolean,
    element:          PsiNamedElement,
    usageType:        UsageType
  ): Option[BeforeIndicesSearchAction] = {
    import DialogWrapper.{CANCEL_EXIT_CODE, OK_EXIT_CODE}

    val title  = usageType.toString
    val dialog = new ImplicitFindUsagesDialog(false, element, title)
    dialog.show()

    dialog.getExitCode match {
      case OK_EXIT_CODE if dialog.shouldCompile =>
        if (validIndexExists) Option(BuildModules(element, project, dirtyModules))
        else                  Option(RebuildIndices(project, element))
      case OK_EXIT_CODE     => None
      case CANCEL_EXIT_CODE => Option(CancelSearch)
    }
  }
}
