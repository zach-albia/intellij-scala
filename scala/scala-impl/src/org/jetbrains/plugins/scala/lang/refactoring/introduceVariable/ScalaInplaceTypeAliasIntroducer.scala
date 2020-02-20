package org.jetbrains.plugins.scala.lang.refactoring.introduceVariable

import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.command.undo.UndoManager
import com.intellij.openapi.editor.{Editor, ScrollType}
import com.intellij.psi._
import com.intellij.refactoring.RefactoringActionHandler
import org.jetbrains.plugins.scala.{ScalaBundle, extensions}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.refactoring.rename.inplace.ScalaMemberInplaceRenamer

/**
 * Created by Kate Ustyuzhanina
 * on 8/10/15
 */
object ScalaInplaceTypeAliasIntroducer {

  def revertState(myEditor: Editor, scopeItem: ScopeItem, namedElement: ScNamedElement): Unit = {
    val myProject = myEditor.getProject
    CommandProcessor.getInstance.executeCommand(myProject, new Runnable {
      def run() {
        val revertInfo = myEditor.getUserData(ScalaIntroduceVariableHandler.REVERT_INFO)
        val document = myEditor.getDocument
        if (revertInfo != null) {
          extensions.inWriteAction {
            document.replaceString(0, document.getTextLength, revertInfo.fileText)
            PsiDocumentManager.getInstance(myProject).commitDocument(document)
          }
          val offset = revertInfo.caretOffset
          myEditor.getCaretModel.moveToOffset(offset)
          myEditor.getScrollingModel.scrollToCaret(ScrollType.MAKE_VISIBLE)
          PsiDocumentManager.getInstance(myEditor.getProject).commitDocument(document)
        }
        if (!myProject.isDisposed && myProject.isOpen) {
          PsiDocumentManager.getInstance(myProject).commitDocument(document)
        }
      }
    }, ScalaBundle.message("command.introduce.type.alias"), null)
  }
}

class ScalaInplaceTypeAliasIntroducer(element: ScNamedElement)
                                     (implicit editor: Editor)
  extends ScalaMemberInplaceRenamer(element, element, editor, element.getName, element.getName) {

  override def setAdvertisementText(text: String): Unit = {
    myAdvertisementText = ScalaBundle.message("press.hotkey.to.show.more.options")
  }

  override def startsOnTheSameElement(handler: RefactoringActionHandler, element: PsiElement): Boolean = {
    def checkEquals(typeAliasDefinition: ScTypeAliasDefinition) = {
      editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO).getNamedElement == element
    }

    element match {
      case typeAliasDefinition: ScTypeAliasDefinition =>
        checkEquals(typeAliasDefinition) && handler.isInstanceOf[ScalaIntroduceVariableHandler]
      case _ => false
    }
  }

  override def revertState(): Unit = {
    //do nothing. we don't need to revert state
  }

  protected override def moveOffsetAfter(success: Boolean): Unit = {
    if (success) {
      // don't know about element to refactor place
    }
    else if (myInsertedName != null && !UndoManager.getInstance(myProject).isUndoInProgress
      && !editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO).isCallModalDialogInProgress) {

      val revertInfo = myEditor.getUserData(ScalaIntroduceVariableHandler.REVERT_INFO)
      if (revertInfo != null) {
        extensions.inWriteAction {
          val myFile: PsiFile = PsiDocumentManager.getInstance(myEditor.getProject).getPsiFile(myEditor.getDocument)
          myEditor.getDocument.replaceString(0, myFile.getTextLength, revertInfo.fileText)
        }
        myEditor.getCaretModel.moveToOffset(revertInfo.caretOffset)
        myEditor.getScrollingModel.scrollToCaret(ScrollType.MAKE_VISIBLE)
      }
    }
  }
}