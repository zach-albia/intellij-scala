package org.jetbrains.plugins.scala
package lang
package psi
package controlFlow
package cfg

import org.jetbrains.plugins.scala.dfa.{DfEntity, DfVariable}
import org.jetbrains.plugins.scala.lang.psi.types.ScType

class New private[controlFlow](val classType: ScType, ret: DfVariable) extends Instruction {
  override def sourceEntities: Seq[DfEntity] = Seq.empty
  override def variables: Seq[DfVariable] = Seq(ret)

  override def asmString: String = Instruction.asmAssignmentPrefix(ret) + s"new ${classType.toString.replace("\n", "")}"

  override def info: Instruction.Info = New
  override def accept(visitor: AbstractInstructionVisitor): Unit = visitor.visitNew(this)
}

object New extends Instruction.Info(name = "New")
