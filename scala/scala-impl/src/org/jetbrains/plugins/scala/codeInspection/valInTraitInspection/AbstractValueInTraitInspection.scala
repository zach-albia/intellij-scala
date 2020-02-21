package org.jetbrains.plugins.scala
package codeInspection
package valInTraitInspection

import com.intellij.codeInspection.{ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScValueDeclaration, ScVariableDeclaration}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait

/**
 * User: Alexander Podkhalyuzin
 * Date: 02.06.2009
 */

class AbstractValueInTraitInspection
  extends AbstractInspection(InspectionBundle.message("display.name.abstract.value.in.trait")) {
  override def actionFor(implicit holder: ProblemsHolder, isOnTheFly: Boolean): PartialFunction[PsiElement, Unit] = {
    //todo: we should use dataflow analysis to get if it's safe to use declaration here
    case v: ScValueDeclaration if v.getParent.isInstanceOf[ScTemplateBody] =>
      v.containingClass match {
        case _: ScTrait =>
          holder.registerProblem(v, InspectionBundle.message("abstract.value.used.in.trait"), ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
        case _ =>
      }
    case v: ScVariableDeclaration =>
      v.containingClass match {
        case _: ScTrait =>
          holder.registerProblem(v, InspectionBundle.message("abstract.variable.used.in.trait"), ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
        case _ =>
      }
  }
}
