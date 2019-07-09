package org.jetbrains.plugins.scala
package annotator
package element

import com.intellij.lang.annotation.AnnotationHolder

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScMacroDefinition
import org.jetbrains.plugins.scala.project.{ProjectPsiElementExt, ScalaLanguageLevel}

private[annotator] object ScMacroDefAnnotator extends ElementAnnotator[ScMacroDefinition] {
  override def annotate(
    element:   ScMacroDefinition,
    typeAware: Boolean
  )(implicit
    holder: AnnotationHolder
  ): Unit =
    if (element.returnTypeElement.isEmpty) {
      val message = ScalaBundle.message("macro.defs.must.have.explicit.return.type")

      if (isScala213OrNewer(element)) holder.createErrorAnnotation(element.nameId, message)
      else                            holder.createWarningAnnotation(element.nameId, message)
    }

  private[this] def isScala213OrNewer(e: PsiElement): Boolean =
    e.scalaLanguageLevel.exists(_ >= ScalaLanguageLevel.Scala_2_13)
}
