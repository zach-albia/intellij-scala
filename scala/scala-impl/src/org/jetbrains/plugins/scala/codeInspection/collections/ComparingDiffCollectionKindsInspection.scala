package org.jetbrains.plugins.scala.codeInspection.collections

import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.InspectionBundle
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/**
 * @author Nikolay.Tropin
 */
class ComparingDiffCollectionKindsInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(ComparingDiffCollectionKinds)
}

object ComparingDiffCollectionKinds extends SimplificationType {
  sealed trait Side {
    def fold[T](ifLeft: => T)(ifRight: => T): T = this match {
      case Side.Left => ifLeft
      case Side.Right => ifRight
    }
  }
  object Side {
    def isLeft(isLeft: Boolean): Side =
      if (isLeft) Left else Right

    case object Right extends Side
    case object Left extends Side
  }

  override def hint: String = InspectionBundle.message("hint.comparing.different.collection.kinds")
  @Nls
  def convertHint(side: Side, toCollection: String): String = side match {
    case Side.Left => InspectionBundle.message("hint.convert.left.hand.side.to.collection", toCollection)
    case Side.Right => InspectionBundle.message("hint.convert.right.hand.side.to.collection", toCollection)
  }

  override def getSimplifications(expr: ScExpression): Seq[Simplification] = expr match {
    case (left @ collectionOfKind(leftKind)) `(!)==` (right @ collectionOfKind(rightKind))
      if leftKind != rightKind =>
      def convertSimplification(side: Side): Seq[Simplification] = {
        val (otherKind, exprToConvert) =
          side.fold(rightKind -> left)(leftKind -> right)
        if (otherKind == "Array") return Seq.empty
        val convertText = partConvertedExprText(expr, exprToConvert, "to" + otherKind)
        Seq(replace(expr).withText(convertText).withHint(convertHint(side, otherKind)).highlightRef)
      }
      convertSimplification(Side.Left) ++ convertSimplification(Side.Right)
    case _ => Seq.empty
  }

  private object collectionOfKind {
    def unapply(expr: ScExpression): Option[String] = {
      expr match {
        case _ if isSeq(expr) => Some("Seq")
        case _ if isSet(expr) => Some("Set")
        case _ if isMap(expr) => Some("Map")
        case _ if isIterator(expr) => Some("Iterator")
        case _ if isArray(expr) => Some("Array")
        case _ => None
      }
    }
  }

  private object `(!)==` {
    def unapply(expr: ScExpression): Option[(ScExpression, ScExpression)] = {
      expr match {
        case left `==` right => Some(left, right)
        case left `!=` right => Some(left, right)
        case _ => None
      }
    }
  }

  private def partConvertedExprText(expr: ScExpression, subExpr: ScExpression, conversion: String) = {
    val subExprConvertedText = subExpr match {
      case _: ScMethodCall | _: ScReferenceExpression | _: ScParenthesisedExpr | _: ScTuple => 
        s"${subExpr.getText}.$conversion" 
      case _ => s"(${subExpr.getText}).$conversion"
    }
    val exprText = expr.getText
    val rangeInParent = subExpr.getTextRange.shiftRight( - expr.getTextOffset)
    val firstPart = exprText.substring(0, rangeInParent.getStartOffset)
    val lastPart = exprText.substring(rangeInParent.getEndOffset)
    s"$firstPart$subExprConvertedText$lastPart"
  }
}
