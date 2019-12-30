package org.jetbrains.plugins.scala.codeInspection.dataFlow

import com.intellij.codeInspection.{LocalInspectionTool, ProblemsHolder}
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition

class ScalaDataFlowInspection extends LocalInspectionTool {

  def analyzeMethod(fun: ScFunctionDefinition): Unit = {
    fun.buildBlockStatementControlFlow()
  }

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = new ScalaElementVisitor {
    override def visitFunctionDefinition(fun: ScFunctionDefinition): Unit = analyzeMethod(fun)
  }
}
