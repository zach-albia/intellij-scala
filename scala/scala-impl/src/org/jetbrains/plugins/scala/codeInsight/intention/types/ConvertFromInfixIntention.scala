package org.jetbrains.plugins.scala
package codeInsight
package intention
package types

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.command.undo.UndoUtil.markPsiFileForUndo
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReference
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScInfixTypeElement, ScParenthesisedTypeElement}

/** Converts type element `(A @@ B)` to `@@[A, B]` */
class ConvertFromInfixIntention extends PsiElementBaseIntentionAction {
  override def getFamilyName: String = ScalaBundle.message("family.name.use.prefix.type.syntax")

  override def getText: String = getFamilyName

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean = {
    element match {
      case Parent((_: ScStableCodeReference) && Parent(Parent(_: ScInfixTypeElement))) => true
      case _ => false
    }
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val infixTypeElement = Option(element).filter(_.isValid)
      .flatMap(_.parentOfType(classOf[ScInfixTypeElement], strict = false))
      .getOrElse(return)

    val replacement = infixTypeElement.computeDesugarizedType
      .getOrElse(return)

    val elementToReplace = infixTypeElement.getParent match {
      case x: ScParenthesisedTypeElement => x
      case _ => infixTypeElement
    }

    elementToReplace.replace(replacement)
    markPsiFileForUndo(replacement.getContainingFile)
  }
}
