package org.jetbrains.plugins.scala.lang.completion
package ml

import java.util

import com.intellij.codeInsight.completion.ml.{CompletionEnvironment, ContextFeatureProvider, MLFeatureValue}
import com.intellij.openapi.util.NotNullLazyKey
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.completion.ml.ScalaContextFeatureProvider._

class ScalaContextFeatureProvider extends ContextFeatureProvider {
  override def getName: String = "scala"

  override def calculateFeatures(environment: CompletionEnvironment): util.Map[String, MLFeatureValue] = {
    val position = Position.getValue(environment)

    val features = new util.HashMap[String, MLFeatureValue]()

    features.put("location", MLFeatureValue.categorical(location(Option(position))))
    features.put("previous_keyword", MLFeatureValue.categorical(previousKeyword(Option(position))))

    features
  }
}

object ScalaContextFeatureProvider {
  private[ml] val Position = NotNullLazyKey.create[PsiElement, CompletionEnvironment]("scala.feature.element.position", environment => {
    positionFromParameters(environment.getParameters)
  })
}
