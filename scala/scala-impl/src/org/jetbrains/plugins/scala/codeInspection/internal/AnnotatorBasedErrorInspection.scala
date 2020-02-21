package org.jetbrains.plugins.scala
package codeInspection
package internal

import com.intellij.codeInsight.daemon.impl.HighlightVisitor
import com.intellij.codeInsight.daemon.impl.analysis.{HighlightInfoHolder, HighlightVisitorImpl}
import com.intellij.codeInspection._
import com.intellij.lang.ASTNode
import com.intellij.lang.annotation._
import com.intellij.lang.injection.InjectedLanguageManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiElementVisitor, PsiJavaFile}
import org.jetbrains.plugins.scala.annotator.{ScalaAnnotation, ScalaAnnotationHolder, ScalaAnnotator}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

/**
 * @author Alexander Podkhalyuzin
 */
final class AnnotatorBasedErrorInspection extends LocalInspectionTool {

  import AnnotatorBasedErrorInspection._

  //noinspection TypeAnnotation
  override def buildVisitor(holder: ProblemsHolder,
                            isOnTheFly: Boolean) = new PsiElementVisitor {

    override def visitElement(element: PsiElement): Unit = {
      implicit val project: Project = element.getProject

      element.getContainingFile match {
        case javaFile: PsiJavaFile => highlightJavaElement(element, javaFile, holder)
        case scalaFile: ScalaFile if !InjectedLanguageManager.getInstance(project).isInjectedFragment(scalaFile) => // todo: remove this after proper support of scala fragments in .md files
          val annotator = new ScalaAnnotator() {
            override def isAdvancedHighlightingEnabled(element: PsiElement): Boolean = true
          }
          annotator.annotate(element)(new DummyAnnotationHolder(element, holder))
        case _ =>
      }
    }
  }
}

object AnnotatorBasedErrorInspection {

  import ProblemHighlightType.{ERROR, GENERIC_ERROR_OR_WARNING}

  private def highlightJavaElement(element: PsiElement,
                                   javaFile: PsiJavaFile,
                                   holder: ProblemsHolder)
                                  (implicit project: Project): Unit = {
    val highlightInfoHolder = new HighlightInfoHolder(javaFile)

    for {
      visitor <- HighlightVisitor.EP_HIGHLIGHT_VISITOR
        .getExtensions(project)
        .headOption

      if visitor.isInstanceOf[HighlightVisitorImpl]
      cloned = visitor.asInstanceOf[HighlightVisitorImpl].clone
    } cloned.analyze(
      javaFile,
      true,
      highlightInfoHolder,
      () => visitor.visit(element)
    )

    if (highlightInfoHolder.hasErrorResults) {
      holder.registerProblem(
        element,
        InspectionBundle.message("error.detected"),
        ERROR,
        null: TextRange
      )
    }
  }

  private class DummyAnnotationHolder(element: PsiElement, holder: ProblemsHolder) extends ScalaAnnotationHolder {

    private val FakeAnnotation = new ScalaAnnotation(new Annotation(
      0,
      0,
      HighlightSeverity.WEAK_WARNING,
      "message",
      "tooltip"
    ))

    override def createAnnotation(severity: HighlightSeverity, range: TextRange, message: String,
                                  htmlTooltip: String): ScalaAnnotation = FakeAnnotation

    def createAnnotation(severity: HighlightSeverity, range: TextRange, str: String): ScalaAnnotation = FakeAnnotation

    def isBatchMode: Boolean = false

    def createInfoAnnotation(range: TextRange, message: String): ScalaAnnotation = FakeAnnotation

    def createInfoAnnotation(node: ASTNode, message: String): ScalaAnnotation = FakeAnnotation

    def createInfoAnnotation(elt: PsiElement, message: String): ScalaAnnotation = FakeAnnotation

    def createInformationAnnotation(range: TextRange, message: String): ScalaAnnotation = FakeAnnotation

    def createInformationAnnotation(node: ASTNode, message: String): ScalaAnnotation = FakeAnnotation

    def createInformationAnnotation(elt: PsiElement, message: String): ScalaAnnotation = FakeAnnotation

    def createWarningAnnotation(range: TextRange, message: String): ScalaAnnotation = {
      holder.registerProblem(element, InspectionBundle.message("warning.with.message", message), GENERIC_ERROR_OR_WARNING)
      FakeAnnotation
    }

    def createWarningAnnotation(node: ASTNode, message: String): ScalaAnnotation = {
      holder.registerProblem(element, InspectionBundle.message("warning.with.message", message), GENERIC_ERROR_OR_WARNING)
      FakeAnnotation
    }

    def createWarningAnnotation(elt: PsiElement, message: String): ScalaAnnotation = {
      holder.registerProblem(element, InspectionBundle.message("warning.with.message", message), GENERIC_ERROR_OR_WARNING)
      FakeAnnotation
    }

    def createErrorAnnotation(range: TextRange, message: String): ScalaAnnotation = {
      if (message != null) {
        holder.registerProblem(element, InspectionBundle.message("error.detected.with.message", message), ERROR)
      }
      FakeAnnotation
    }

    def createErrorAnnotation(node: ASTNode, message: String): ScalaAnnotation = {
      if (message != null) {
        holder.registerProblem(element, InspectionBundle.message("error.detected.with.message", message), ERROR)
      }
      FakeAnnotation
    }

    def createErrorAnnotation(elt: PsiElement, message: String): ScalaAnnotation = {
      if (message != null) {
        holder.registerProblem(element, InspectionBundle.message("error.detected.with.message", message), ERROR)
      }
      FakeAnnotation
    }

    def getCurrentAnnotationSession: AnnotationSession = {
      new AnnotationSession(element.getContainingFile)
    }

    def createWeakWarningAnnotation(p1: TextRange, p2: String): ScalaAnnotation = FakeAnnotation

    def createWeakWarningAnnotation(p1: ASTNode, p2: String): ScalaAnnotation = FakeAnnotation

    def createWeakWarningAnnotation(p1: PsiElement, p2: String): ScalaAnnotation = FakeAnnotation
  }

}