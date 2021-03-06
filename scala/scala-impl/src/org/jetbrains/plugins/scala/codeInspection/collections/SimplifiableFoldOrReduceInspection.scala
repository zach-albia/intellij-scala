package org.jetbrains.plugins.scala
package codeInspection.collections

import org.jetbrains.plugins.scala.codeInspection.InspectionBundle
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * Nikolay.Tropin
 * 2014-05-05
 */
class SimplifiableFoldOrReduceInspection extends OperationOnCollectionInspection {
  val foldSum = new FoldSimplificationType(this, "0", "+", "sum") {
    override def hint: String = InspectionBundle.message("fold.sum.hint")
    override def description: String = InspectionBundle.message("fold.sum.short")
  }
  val foldProduct = new FoldSimplificationType(this, "1", "*", "product") {
    override def hint: String = InspectionBundle.message("fold.product.hint")
    override def description: String = InspectionBundle.message("fold.product.short")
  }
  val reduceSum = new ReduceSimplificationType(this, "+", "sum") {
    override def hint: String = InspectionBundle.message("reduce.sum.hint")
    override def description: String = InspectionBundle.message("reduce.sum.short")
  }
  val reduceProduct = new ReduceSimplificationType(this,"*", "product") {
    override def hint: String = InspectionBundle.message("reduce.product.hint")
    override def description: String = InspectionBundle.message("reduce.product.short")
  }
  val reduceMin = new ReduceSimplificationType(this, "min", "min") {
    override def hint: String = InspectionBundle.message("reduce.min.hint")
    override def description: String = InspectionBundle.message("reduce.min.short")
  }
  val reduceMax = new ReduceSimplificationType(this, "max", "max") {
    override def hint: String = InspectionBundle.message("reduce.max.hint")
    override def description: String = InspectionBundle.message("reduce.max.short")
  }

  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(foldSum, foldProduct, reduceSum, reduceProduct, reduceMax, reduceMin)
}

object SimplifiableFoldOrReduceInspection {

}

abstract class FoldSimplificationType(inspection: OperationOnCollectionInspection,
                             startElem: String,
                             opName: String,
                             methodName: String) extends SimplificationType {

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case qual`.fold`(literal(`startElem`), binaryOperation(`opName`)) if implicitParameterExistsFor(methodName, qual) =>
        val simpl = replace(expr).withText(invocationText(qual, methodName)).highlightFrom(qual)
        Some(simpl)
      case _ => None
    }
  }
}

abstract class ReduceSimplificationType(inspection: OperationOnCollectionInspection,
                               opName: String,
                               methodName: String) extends SimplificationType {
  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case qual`.reduce`(binaryOperation(`opName`)) if implicitParameterExistsFor(methodName, qual) =>
        val simpl = replace(expr).withText(invocationText(qual, methodName)).highlightFrom(qual)
        Some(simpl)
      case _ => None
    }
  }
}