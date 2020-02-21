package org.jetbrains.plugins.scala.annotator.hints

import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import com.intellij.psi.{PsiElement, PsiManager}
import org.jetbrains.plugins.scala.annotator.ScalaHighlightingMode
import org.jetbrains.plugins.scala.annotator.hints.AnnotatorHints.AnnotatorHintsKey
import org.jetbrains.plugins.scala.extensions.PsiElementExt

// Annotator hints, SCL-15593
case class AnnotatorHints(hints: Seq[Hint], modificationCount: Long) {
  def putTo(element: PsiElement): Unit = {
    val showCompilerErrors =
      Option(element.getContainingFile).exists(ScalaHighlightingMode.isShowErrorsFromCompilerEnabled)
    if (!showCompilerErrors)
      element.putUserData(AnnotatorHintsKey, this)
  }
}

object AnnotatorHints {
  private val AnnotatorHintsKey = Key.create[AnnotatorHints]("AnnotatorHints")

  def in(element: PsiElement): Option[AnnotatorHints] = Option(element.getUserData(AnnotatorHintsKey))

  def clearIn(element: PsiElement): Unit = {
    element.putUserData(AnnotatorHintsKey, null)
  }

  def clearIn(project: Project): Unit = {
    for (editor <- EditorFactory.getInstance().getAllEditors;
         file <- Option(FileDocumentManager.getInstance().getFile(editor.getDocument));
         psiFile <- Option(PsiManager.getInstance(project).findFile(file))) {
      psiFile.elements.foreach(clearIn)
    }
  }
}