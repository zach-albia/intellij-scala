package org.jetbrains.plugins.scala.caches

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.{ModificationTracker, SimpleModificationTracker}
import com.intellij.psi.impl.compiled.ClsFileImpl
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScModificationTrackerOwner

import scala.annotation.tailrec

/**
  * Nikolay.Tropin
  * 04-Jun-18
  */
object Tracker {

  def semanticChange(element: PsiElement): ModificationTracker = libraryAwareBlockModTracker(element)

  def anyPhysicalPsiChange(project: Project): ModificationTracker = PsiManager.getInstance(project).getModificationTracker

  def rootsChange(project: Project): ModificationTracker = ProjectRootManager.getInstance(project)

  def globalStructureChange(project: Project): ModificationTracker =
    PsiManager.getInstance(project).getModificationTracker.getJavaStructureModificationTracker

  /** Updates on every (including non-physical) scala psi modification
    *
    * Use for hot methods: it has minimal overhead
    *
    * PsiModificationTracker is not an option, because it
    * - requires calling getProject first
    * - doesn't work for non-physical elements
    */
  def anyScalaPsiChange: SimpleModificationTracker = AnyScalaPsiModificationTracker

  private def libraryAwareBlockModTracker(element: PsiElement): ModificationTracker = {
    val project = element.getProject
    val file = element.getContainingFile

    if (isLibraryFile(file)) rootsChange(project) else blockModTracker(element)
  }

  private def isLibraryFile(file: PsiFile) = file match {
    case file: ScalaFile if file.isCompiled =>
      ProjectRootManager.getInstance(file.getProject)
        .getFileIndex
        .isInLibrary(file.getVirtualFile)
    case cls: ClsFileImpl => true
    case _ => false
  }

  private def blockModTracker(elem: PsiElement): ModificationTracker = {
    @tailrec
    def calc(element: PsiElement): ModificationTracker = {
      PsiTreeUtil.getContextOfType(element, false, classOf[ScModificationTrackerOwner]) match {
        case null => globalStructureChange(elem.getProject)
        case owner@ScModificationTrackerOwner() =>
          new ModificationTracker {
            override def getModificationCount: Long = owner.modificationCount
          }
        case owner => calc(owner.getContext)
      }
    }

    calc(elem)
  }

  private object AnyScalaPsiModificationTracker extends SimpleModificationTracker
}
