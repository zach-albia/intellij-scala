package org.jetbrains.plugins.scala.codeInspection.collections

import org.jetbrains.plugins.scala.codeInspection.InspectionBundle
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * @author Nikolay.Tropin
 */
class RangeToIndicesInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(RangeToIndices, UntilToIndices, ToToIndices)
}

object RangeToIndices extends SimplificationType {
  override def hint: String = InspectionBundle.message("hint.replace.with.indices")
  //noinspection ScalaExtractStringToBundle
  override def description: String = "Range(0, seq.size)"

  private val Range = invocation("apply").from(Array("scala.collection.immutable.Range"))

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case Range(_, literal("0"), qual`.sizeOrLength`()) if isSeq(qual) || isArray(qual) => toIndicesSimplification(expr, qual)
    case _ => None
  }

  def toIndicesSimplification(expr: ScExpression, qual: ScExpression): Some[Simplification] = {
    Some(replace(expr)
      .withText(invocationText(qual, "indices"))
      .withHint(InspectionBundle.message("hint.replace.with.indices.with.preview", qual.getText))
      .highlightAll
    )
  }
}

object UntilToIndices extends SimplificationType {
  //noinspection ScalaExtractStringToBundle
  override def hint: String = "0 until seq.size"

  private val `.until` = invocation("until").from(Array("scala.runtime.RichInt"))

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case literal("0")`.until`(qual`.sizeOrLength`())  if isSeq(qual) || isArray(qual) =>
      RangeToIndices.toIndicesSimplification(expr, qual)
    case _ => None
  }

}

object ToToIndices extends SimplificationType {
  //noinspection ScalaExtractStringToBundle
  override def hint: String = "0 to (seq.size - 1)"

  private val `.to` = invocation("to").from(Array("scala.runtime.RichInt"))

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case literal("0")`.to`(qual`.sizeOrLength`() `-` literal("1"))  if isSeq(qual) || isArray(qual) =>
      RangeToIndices.toIndicesSimplification(expr, qual)
    case _ => None
  }
}
