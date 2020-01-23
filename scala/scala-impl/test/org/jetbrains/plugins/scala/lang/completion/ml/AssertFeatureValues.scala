package org.jetbrains.plugins.scala.lang.completion.ml

import com.intellij.codeInsight.completion.ml.MLFeatureValue
import org.junit.Assert

private[ml] object AssertFeatureValues {

  def equals(expected: MLFeatureValue, actual: MLFeatureValue): Unit = {
    // no equals impl for MLFeatureValue
    def adapter(value: MLFeatureValue): Any = {
      val actualValue = Option(value.asBinary()) orElse Option(value.asCategorical()) orElse Option(value.asFloat())
      actualValue.get
    }

    val adaptedExpected = adapter(expected)
    val adaptedActual = adapter(actual)

    adaptedExpected match {
      case floatExpected: Double => Assert.assertEquals(floatExpected, adaptedActual.asInstanceOf[Double], 0.001)
      case _ => Assert.assertEquals(adaptedExpected, adaptedActual)
    }
  }
}
