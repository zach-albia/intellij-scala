package org.jetbrains.plugins.scala.codeInsight.intention

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions.Parent
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory


/**
  * mattfowler
  * 5/21/2016
  */
class CreateCompanionObjectIntention extends PsiElementBaseIntentionAction {
  override def getText: String = ScalaBundle.message("create.companion.object.for.class")

  override def invoke(project: Project, editor: Editor, psiElement: PsiElement): Unit = {
    getClassIfAvailable(psiElement).foreach { clazz =>
      val companion = ScalaPsiElementFactory.createObjectWithContext(
        s"""|object ${clazz.name} {
            |
            |}""".stripMargin, psiElement.getContext, psiElement)
      val parent = clazz.getParent
      val obj = parent.addAfter(companion, psiElement.getParent)
      if (ScalaCodeStyleSettings.getInstance(project).USE_SCALAFMT_FORMATTER)
        parent.addAfter(ScalaPsiElementFactory.createWhitespace("\n")(project), psiElement.getParent)
      moveCaret(project, editor, obj)
    }
  }

  override def isAvailable(project: Project, editor: Editor, psiElement: PsiElement): Boolean =
    getClassIfAvailable(psiElement).exists {
      _.baseCompanionModule.isEmpty
    }

  private def moveCaret(project: Project, editor: Editor, obj: PsiElement): Unit = {
    val document = editor.getDocument
    PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
    val startOffset = obj.getTextRange.getStartOffset
    val nextLine = document.getLineNumber(startOffset) + 1
    CodeStyleManager.getInstance(project).adjustLineIndent(document, document.getLineStartOffset(nextLine))
    editor.getCaretModel.moveToOffset(document.getLineEndOffset(nextLine))
  }

  private def getClassIfAvailable(psiElement: PsiElement): Option[ScTypeDefinition] = {
    psiElement match {
      case Parent(td: ScTypeDefinition) if psiElement == td.nameId && !td.isObject => Some(td)
      case _ => None
    }
  }

  override def getFamilyName: String = CreateCompanionObjectIntention.getFamilyName
}


object CreateCompanionObjectIntention {
  def getFamilyName: String = "Create companion object"
}