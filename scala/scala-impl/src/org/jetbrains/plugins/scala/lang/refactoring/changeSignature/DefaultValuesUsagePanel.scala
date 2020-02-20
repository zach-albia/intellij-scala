package org.jetbrains.plugins.scala
package lang.refactoring.changeSignature

import java.awt.event.{ItemEvent, ItemListener}

import javax.swing._
import org.jetbrains.plugins.scala.ScalaBundle

/**
 * Nikolay.Tropin
 * 2014-09-05
 */
class DefaultValuesUsagePanel(labelText: String = ScalaBundle.message("default.values")) extends JPanel {
  private val myRbModifyCalls: JRadioButton = new JRadioButton
  private val myRbAddDefaultArg: JRadioButton = new JRadioButton

  init()

  def init() {
    val boxLayout: BoxLayout = new BoxLayout(this, BoxLayout.X_AXIS)
    setLayout(boxLayout)

    add(new JLabel(labelText))
    myRbAddDefaultArg.setText(ScalaBundle.message("add.to.definition"))
    myRbModifyCalls.setText(ScalaBundle.message("modify.method.calls"))
    myRbAddDefaultArg.setMnemonic('d')
    myRbModifyCalls.setMnemonic('m')
    myRbModifyCalls.setSelected(true)

    add(myRbModifyCalls)
    add(myRbAddDefaultArg)
    add(Box.createHorizontalGlue)
    

    val bg: ButtonGroup = new ButtonGroup
    Seq(myRbAddDefaultArg, myRbModifyCalls).foreach(bg.add)

    val listener = new ItemListener {
      def itemStateChanged(e: ItemEvent) {
        stateModified()
      }
    }
    myRbModifyCalls.addItemListener(listener)
    myRbAddDefaultArg.addItemListener(listener)
  }


  protected def stateModified() {
  }

  def isAddDefaultArgs: Boolean = myRbAddDefaultArg.isSelected

  def isModifyCalls: Boolean = myRbModifyCalls.isSelected

  def forceIsModifyCalls(): Unit = {
    myRbModifyCalls.setSelected(true)
    myRbModifyCalls.setEnabled(false)
    myRbAddDefaultArg.setEnabled(false)
  }

  def release(): Unit = {
    myRbModifyCalls.setEnabled(true)
    myRbAddDefaultArg.setEnabled(true)
  }
}
