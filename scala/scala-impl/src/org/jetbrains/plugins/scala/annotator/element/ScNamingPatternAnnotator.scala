package org.jetbrains.plugins.scala.annotator.element

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.codeInspection.ProblemHighlightType
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScNamingPattern, ScSeqWildcard}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.ScalaLanguageLevel

object ScNamingPatternAnnotator extends ElementAnnotator[ScNamingPattern] {

  override def annotate(pattern: ScNamingPattern, typeAware: Boolean = true)
                       (implicit holder: AnnotationHolder): Unit = {

    if (pattern.scalaLanguageLevel.forall(_ < ScalaLanguageLevel.Scala_3_0)) {
      scala3StyleVarargPatternColon(pattern) match {
        case Some(colon) =>
          val annotation = holder.createWarningAnnotation(colon, ScalaBundle.message("vararg.pattern.with.colon.requires.scala3"))
          annotation.setHighlightType(ProblemHighlightType.GENERIC_ERROR)
          annotation.registerFix(new ReplaceWithAtFix(colon))
        case _ =>
      }
    }
  }

  private def scala3StyleVarargPatternColon(pattern: ScNamingPattern): Option[PsiElement] =
    for {
      naming      <- Some(pattern).filterByType[ScNamingPattern]
      seqWildcard <- Some(naming.named).filterByType[ScSeqWildcard]
      colon       <- Option(seqWildcard.getPrevSiblingNotWhitespace)
      if colon.elementType == ScalaTokenTypes.tCOLON
    } yield colon

  private class ReplaceWithAtFix(colon: PsiElement) extends IntentionAction {

    override def getText: String = "Replace with '@'"

    override def getFamilyName: String = "Replace ':' with '@' in vararg pattern"

    override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = true

    override def startInWriteAction(): Boolean = true

    override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
      if (!colon.isValid) return
      val pattern = ScalaPsiElementFactory.createPatternFromText("List(_ @ _*)")(project)
      val at = pattern.elements.find(_.elementType == ScalaTokenTypes.tAT)
      at.foreach(colon.replace)
    }
  }
}
