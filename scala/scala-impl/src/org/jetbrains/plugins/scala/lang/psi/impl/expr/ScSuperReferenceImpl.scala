package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import com.intellij.openapi.util.TextRange
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.IncorrectOperationException
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createIdentifier
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScTypeExt}
import org.jetbrains.plugins.scala.lang.resolve.{ResolveUtils, ScalaResolveResult}

/**
  * @author Alexander Podkhalyuzin
  *         Date: 14.03.2008
  */
class ScSuperReferenceImpl(node: ASTNode) extends ScExpressionImplBase(node) with ScSuperReference {
  def isHardCoded: Boolean = {
    val id = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER)
    if (id == null) false else {
      ScalaPsiUtil.fileContext(id) match {
        case file: ScalaFile if file.isCompiled =>
          val next = id.getNode.getTreeNext
          if (next == null) false
          else next.getPsi match {
            case comment: PsiComment =>
              val commentText = comment.getText
              val path = commentText.substring(2, commentText.length - 2)
              val classes = ScalaPsiManager.instance(getProject).getCachedClasses(getResolveScope, path)
              if (classes.length == 1) {
                drvTemplate.exists(td => !ScalaPsiUtil.isInheritorDeep(td, classes(0)))
              } else {
                val clazz: Option[PsiClass] = classes.find(!_.isInstanceOf[ScObject])
                clazz match {
                  case Some(psiClass) =>
                    drvTemplate.exists(td => !ScalaPsiUtil.isInheritorDeep(td, psiClass))
                  case _ => false
                }
              }
            case _ => false
          }
        case _ => false
      }
    }
  }

  def drvTemplate: Option[ScTemplateDefinition] = reference match {
    case Some(q) => q.bind() match {
      case Some(ScalaResolveResult(td: ScTypeDefinition, _)) => Some(td)
      case _ => None
    }
    case None => ScalaPsiUtil.drvTemplate(this)
  }


  def staticSuper: Option[ScType] = {
    val id = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER)
    if (id == null) None else findSuper(id)
  }

  def staticSuperName = Option(findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER)).map(_.getText).getOrElse("")

  override def getReference = {
    val id = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER)
    if (id == null) null else new PsiReference {
      def getElement: ScSuperReferenceImpl = ScSuperReferenceImpl.this

      def getRangeInElement: TextRange = new TextRange(0, id.getTextLength).shiftRight(id.getStartOffsetInParent)

      def getCanonicalText: String = resolve match {
        case c: PsiClass => c.qualifiedName
        case _ => null
      }

      def isSoft: Boolean = false

      def handleElementRename(newElementName: String): ScSuperReferenceImpl = doRename(newElementName)

      def bindToElement(e: PsiElement): ScSuperReferenceImpl = e match {
        case c: PsiClass => doRename(c.name)
        case _ => throw new IncorrectOperationException("cannot bind to anything but class")
      }

      private def doRename(newName: String) = {
        val parent = id.getNode.getTreeParent
        parent.replaceChild(id.getNode, createIdentifier(newName))
        ScSuperReferenceImpl.this
      }

      def isReferenceTo(element: PsiElement): Boolean = element match {
        case c: PsiClass => c.name == id.getText && resolve == c
        case _ => false
      }

      def resolve: PsiClass = {
        def resolveNoHack: PsiClass = {
          findSuper(id) match {
            case Some(t) => t.extractClass match {
              case Some(c) => c
              case None => null
            }
            case _ => null
          }
        }

        ScalaPsiUtil.fileContext(id) match {
          case file: ScalaFile if file.isCompiled =>
            val next = id.getNode.getTreeNext
            if (next == null) resolveNoHack
            else next.getPsi match {
              case comment: PsiComment =>
                val commentText = comment.getText
                val path = commentText.substring(2, commentText.length - 2)
                val classes = ScalaPsiManager.instance(getProject).getCachedClasses(getResolveScope, path)
                if (classes.length == 1) classes(0)
                else classes.find(!_.isInstanceOf[ScObject]).getOrElse(resolveNoHack)
              case _ => resolveNoHack
            }
          case _ => resolveNoHack
        }
      }

      override def getVariants: Array[Object] = superTypes match {
        case None => Array.emptyObjectArray
        case Some(supers) =>
          supers.flatMap(_.extractClass).toArray
      }
    }
  }

  def findSuper(id: PsiElement): Option[ScType] = superTypes match {
    case None => None
    case Some(types) =>
      val name = id.getText
      for (t <- types) {
        t.extractClass match {
          case Some(c) if name == c.name => return Some(t)
          case _ =>
        }
      }
      None
  }

  private def superTypes: Option[Seq[ScType]] = reference match {
    case Some(q) =>
      q.resolve() match {
        case clazz: PsiClass => Some(clazz.getSuperTypes.map(_.toScType()))
        case _               => None
      }
    case None => ResolveUtils.enclosingTypeDef(this).map(_.extendsBlock.superTypes)
  }

  protected override def innerType = Failure(ScalaBundle.message("cannot.infer.type.of.super.expression"))

  override def toString = "SuperReference"
}