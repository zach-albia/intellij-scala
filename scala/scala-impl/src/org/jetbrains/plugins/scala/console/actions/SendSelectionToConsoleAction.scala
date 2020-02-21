package org.jetbrains.plugins.scala.console.actions

import java.io.{IOException, OutputStream}

import com.intellij.openapi.actionSystem._
import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.scala.console.{ScalaConsoleInfo, ScalaLanguageConsole}
import org.jetbrains.plugins.scala.extensions
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

class SendSelectionToConsoleAction extends AnAction {

  override def update(e: AnActionEvent): Unit = {
    val presentation = e.getPresentation
    presentation.setIcon(Icons.SCALA_CONSOLE)

    def enable(): Unit = {
      presentation.setEnabled(true)
      presentation.setVisible(true)
    }

    def disable(): Unit = {
      presentation.setEnabled(false)
      presentation.setVisible(false)
    }

    try {
      val context = e.getDataContext
      val file = CommonDataKeys.PSI_FILE.getData(context)
      if (file == null) {
        disable()
        return
      }
      val editor = CommonDataKeys.EDITOR.getData(context)
      val hasSelection = editor.getSelectionModel.hasSelection
      val console = ScalaConsoleInfo.getConsole(file.getProject)

      if (!hasSelection || console == null) {
        disable()
        return
      }

      val consoleEditor = console.getConsoleEditor
      if (consoleEditor == null || consoleEditor.isDisposed) {
        disable()
        return
      }

      val processHandler = ScalaConsoleInfo.getProcessHandler(file.getProject)
      if (processHandler == null || processHandler.isProcessTerminated) {
        disable()
        return
      }

      file match {
        case _: ScalaFile => enable()
        case _ => disable()
      }
    }
    catch {
      case _: Exception => disable()
    }
  }

  override def actionPerformed(e: AnActionEvent): Unit = {
    val context = e.getDataContext
    val editor = CommonDataKeys.EDITOR.getData(context)
    val project = CommonDataKeys.PROJECT.getData(context)

    if (editor == null || project == null) return
    val selectedText = editor.getSelectionModel.getSelectedText
    val console = ScalaConsoleInfo.getConsole(project)
    if (console != null) sendSelection(console, selectedText)
  }

  def sendSelection(console: ScalaLanguageConsole, text: String): Unit = {
    val consoleEditor = console.getConsoleEditor
    val controller = ScalaConsoleInfo.getController(console.getProject)
    val processHandler = ScalaConsoleInfo.getProcessHandler(console.getProject)

    if (consoleEditor != null) {
      val document = console.getEditorDocument
      console.setInputText(text)

      extensions.inWriteAction {
        val range: TextRange = new TextRange(0, document.getTextLength)
        consoleEditor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
        console.addToHistory(range, console.getConsoleEditor, true)
        controller.addToHistory(text)

        consoleEditor.getCaretModel.moveToOffset(0)
        consoleEditor.getDocument.setText("")
      }

      text.split('\n').foreach(line => {
        if (line != "") {
          val outputStream: OutputStream = processHandler.getProcessInput
          try {
            val bytes: Array[Byte] = (line + "\n").getBytes
            outputStream.write(bytes)
            outputStream.flush()
          }
          catch {
            case _: IOException => //ignore
          }
        }
        console.textSent(line + "\n")
      })
    }
  }
}


