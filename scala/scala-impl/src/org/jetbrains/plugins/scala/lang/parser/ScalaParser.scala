package org.jetbrains.plugins.scala
package lang
package parser

import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType

final class ScalaParser(isScala3: Boolean = false) extends PsiParser {

  import parsing._
  import builder.ScalaPsiBuilderImpl

  override def parse(root: IElementType, delegate: PsiBuilder): ASTNode = {
    val builder = new ScalaPsiBuilderImpl(delegate, isScala3)

    root match {
      case ScCodeBlockElementType.BlockExpression =>
        expressions.BlockExpr.parse(builder)
      case _ =>
        val rootMarker = delegate.mark()
        Program.parse(builder)
        rootMarker.done(root)
    }

    delegate.getTreeBuilt
  }
}
