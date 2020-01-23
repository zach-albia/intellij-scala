package org.jetbrains.plugins.scala.lang.completion.ml

import java.util

import com.intellij.codeInsight.completion.ml.{CompletionEnvironment, ContextFeatureProvider, MLFeatureValue}
import com.intellij.codeInsight.completion.{CodeCompletionHandlerBase, CompletionType}
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.junit.{Assert, Test}

class ScalaContextFeatureProviderTest extends ScalaLightCodeInsightFixtureTestAdapter {

  @Test
  def testLocation(): Unit = {
    import org.jetbrains.plugins.scala.lang.completion.ml.Location._

    assertFeature("location", MLFeatureValue.categorical(CLASS_BODY))(
      """object X {
        |  <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(CLASS_BODY))(
      """class X {
        |  <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(CLASS_BODY))(
      """trait X {
        |  <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(CLASS_BODY))(
      """trait X {
        |  private <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(CLASS_PARENTS))(
      """class X extends <caret> {
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(FILE))(
      """<caret>
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(FILE))(
      """trait X {
        |
        |}
        |
        |<caret>
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """object X {
        |  if (true) <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(BLOCK))(
      """object X {
        |  if (true) {
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(IF))(
      """object X {
        |  if (<caret>)
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(FOR))(
      """object X {
        |  for {
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(FOR))(
      """object X {
        |  for (<caret>)
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """object X {
        |  for {
        |    _ <- List.empty
        |  }
        |  yield <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(BLOCK))(
      """object X {
        |  for {
        |    _ <- List.empty
        |  }
        |  yield {
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """object X {
        |  try <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(BLOCK))(
      """object X {
        |  try {
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(BLOCK))(
      """object X {
        |  try {
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """object X {
        |  try {
        |    throw new Exception
        |  }
        |  finally <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(BLOCK))(
      """class X {
        |  def f(): Unit = {
        |    2 + 2 == 5
        |    <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(ARGUMENT))(
      """class X {
        |  println(<caret>)
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(ARGUMENT))(
      """class X {
        |  def f(): Unit = {
        |    1 + <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(REFERENCE))(
      """class X {
        |  def f(): Unit = {
        |    1.<caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(REFERENCE))(
      """object X {
        |  val a = 1 <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(REFERENCE))(
      """class X {
        |  List.empty[Int].map(_.<caret>)
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """class X {
        |  def f(): Unit = <caret>
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(EXPRESSION))(
      """class X {
        |  def f(): Unit = {
        |    val a = <caret>
        |  }
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(PARAMETER))(
      """class X {
        |  def f(<caret>): Unit
        |}
        |""".stripMargin
    )

    assertFeature("location", MLFeatureValue.categorical(UNKNOWN))(
      """class X {
        |  def f(): Unit = <caret> + 1
        |}
        |""".stripMargin
    )
  }

  @Test
  def testPreviousKeyword(): Unit = {
    import org.jetbrains.plugins.scala.lang.completion.ml.Keyword._

    assertFeature("previous_keyword", MLFeatureValue.categorical(PRIVATE))(
      """trait X {
        |  private <caret>
        |}
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(ABSRACT))(
      """class X {
        |  protected abstract <caret>
        |}
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(IMPLICIT))(
      """class X {
        |  protected abstract implicit <caret>
        |}
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(DEF))(
      """class X {
        |  protected abstract implicit def <caret>
        |}
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(LAZY))(
      """class X {
        |  lazy <caret>
        |}
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(IMPORT))(
      """import <caret>
        |""".stripMargin
    )

    assertFeature("previous_keyword", MLFeatureValue.categorical(ELSE))(
      """object X {
        |  if (true) 1
        |  else <caret>
        |}
        |""".stripMargin
    )
  }

  private def assertFeature(name: String, expected: MLFeatureValue)(fileText: String): Unit = {
    val features = computeFeatures(fileText)
    assertFeatureEquals(expected, features.get(name))
  }

  private def computeFeatures(fileText: String): util.Map[String, MLFeatureValue] = {
    class ScalaContextFeatureProviderWrapper extends ContextFeatureProvider {

      private val original = new ScalaContextFeatureProvider

      var features: util.Map[String, MLFeatureValue] = _

      override def getName: String = original.getName

      override def calculateFeatures(environment: CompletionEnvironment): util.Map[String, MLFeatureValue] = {
        val result = original.calculateFeatures(environment)

        features = result

        result
      }
    }

    val provider = new ScalaContextFeatureProviderWrapper
    try {
      ContextFeatureProvider.EP_NAME.addExplicitExtension(ScalaLanguage.INSTANCE, provider)

      configureFromFileText(fileText)
      changePsiAt(getEditor.getCaretModel.getOffset)
      getFixture.complete(CompletionType.BASIC, 1)

      val handler = new CodeCompletionHandlerBase(CompletionType.BASIC, false, false, true)
      handler.invokeCompletion(getProject, getEditor, 1)

      provider.features
    }
    finally {
      ContextFeatureProvider.EP_NAME.removeExplicitExtension(ScalaLanguage.INSTANCE, provider)
    }
  }

  private def assertFeatureEquals(expected: MLFeatureValue, actual: MLFeatureValue): Unit = {

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
