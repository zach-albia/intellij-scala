package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

class MapGetEmptinessCheckInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(MapGetNonEmpty, MapGetIsEmpty)
}

object MapGetNonEmpty extends SimplificationType() {

  override def hint: String = InspectionBundle.message("replace.get.nonEmpty.with.contains")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case map`.getOnMap`(key)`.isDefined`() =>
        Some(replace(expr)
          .withText(invocationText(negation = false, map, "contains", key))
          .highlightFrom(map))
      case _ => None
    }
  }
}

object MapGetIsEmpty extends SimplificationType() {

  override def hint: String = InspectionBundle.message("replace.get.isEmpty.with.not.contains")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case map`.getOnMap`(key)`.isEmpty`() =>
        Some(replace(expr)
          .withText(invocationText(negation = true, map, "contains", key))
          .highlightFrom(map))
      case _ => None
    }
  }
}