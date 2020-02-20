package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base
package patterns

import com.intellij.lang.ASTNode
import com.intellij.psi._
import com.intellij.psi.scope.PsiScopeProcessor
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScBindingPatternStub
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScTypeExt}

/**
 * @author Alexander Podkhalyuzin
 */

class ScNamingPatternImpl private(stub: ScBindingPatternStub[ScNamingPattern], node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, ScalaElementType.NAMING_PATTERN, node) with ScPatternImpl with ScNamingPattern {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScBindingPatternStub[ScNamingPattern]) = this(stub, null)

  override def isIrrefutableFor(t: Option[ScType]): Boolean = named.isIrrefutableFor(t)

  override def toString: String = "NamingPattern: " + ifReadAllowed(name)("")

  def nameId: PsiElement = findChildByType[PsiElement](TokenSets.ID_SET)

  def isWildcard: Boolean = findChildByType[PsiElement](ScalaTokenTypes.tUNDER) != null

  override def `type`(): TypeResult = {
    if (getLastChild.isInstanceOf[ScSeqWildcard]) {
      return this.expectedType match {
        case Some(x) => Right(x)
        case _ =>  Failure(ScalaBundle.message("no.expected.type.for.wildcard.naming"))
      }
    }
    if (named == null) Failure(ScalaBundle.message("cannot.infer.type"))
    else {
      this.expectedType match {
        case Some(expectedType) => named.`type`().map(expectedType.glb(_))
        case _ => named.`type`()
      }
    }
  }

  override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState, lastParent: PsiElement,
                                   place: PsiElement): Boolean = {
    if (isStable) {
      ScalaPsiUtil.processImportLastParent(processor, state, place, lastParent, `type`())
    } else true
  }

  override def getOriginalElement: PsiElement = super[ScNamingPattern].getOriginalElement
}