package org.jetbrains.plugins.scala.lang.completion.ml

import java.util

import com.intellij.codeInsight.completion.ml.{ContextFeatures, ElementFeatureProvider, MLFeatureValue}
import com.intellij.codeInsight.completion.{CodeCompletionHandlerBase, CompletionLocation, CompletionType}
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.psi.PsiNamedElement
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.extensions._

import scala.collection.mutable

class ScalaElementFeatureProviderTest extends ScalaLightCodeInsightFixtureTestAdapter {

  def testPostfix(): Unit = {

    assertContext("postfix", MLFeatureValue.binary(true))(
      s"""object X {
         |  val a = 1
         |  a $CARET
         |}
         |""".stripMargin
    )

    assertContext("postfix", MLFeatureValue.binary(false))(
      s"""object X {
         |  val a = 1
         |  a.$CARET
         |}
         |""".stripMargin
    )
  }

  def testInsideCatch(): Unit = {

    assertContext("inside_catch", MLFeatureValue.binary(true))(
      s"""object X {
         |  try a
         |  catch $CARET
         |}
         |""".stripMargin
    )

    assertContext("inside_catch", MLFeatureValue.binary(false))(
      s"""object X {
         |  try $CARET
         |  catch b
         |}
         |""".stripMargin
    )
  }

  def testTypeExpected(): Unit = {
    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  List[$CARET]
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  type A = $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  val a: $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  class A extends $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  type A = Int with $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  1 match { case _: $CARET }
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(true))(
      s"""object X {
         |  def f(): $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(false))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(false))(
      s"""object X {
         |  val a = $CARET
         |}
         |""".stripMargin
    )

    assertContext("type_expected", MLFeatureValue.binary(false))(
      s"""object X {
         |  1.$CARET
         |}
         |""".stripMargin
    )
  }

  def testAfterNew(): Unit = {
    assertContext("after_new", MLFeatureValue.binary(true))(
      s"""object X {
         |  new $CARET
         |}
         |""".stripMargin
    )

    assertContext("after_new", MLFeatureValue.binary(false))(
      s"""object X {
         |  new java.util.HashMap($CARET)
         |}
         |""".stripMargin
    )
  }

  def testKind(): Unit = {
    import CompletionItem._

    assertElement("kind", "type", MLFeatureValue.categorical(KEYWORD))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "Nil", MLFeatureValue.categorical(VALUE))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "BufferedIterator", MLFeatureValue.categorical(TYPE_ALIAS))(
      s"""object X {
         |  type a = $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "a", MLFeatureValue.categorical(VARIABLE))(
      s"""object X {
         |  var a = 1
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "X", MLFeatureValue.categorical(OBJECT))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "X", MLFeatureValue.categorical(CLASS))(
      s"""class X {
         |  type a = $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "X", MLFeatureValue.categorical(TRAIT))(
      s"""trait X {
         |  type a = $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "int2Integer", MLFeatureValue.categorical(FUNCTION))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "asInstanceOf", MLFeatureValue.categorical(SYNTHETHIC_FUNCTION))(
      s"""object X {
         |  "" $CARET
         |}
         |""".stripMargin
    )


    assertElement("kind", "LinkageError", MLFeatureValue.categorical(EXCEPTION))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("kind", "java", MLFeatureValue.categorical(PACKAGE))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )
  }

  def testKeyword(): Unit = {
    import Keyword._

    assertElement("keyword", "import", MLFeatureValue.categorical(IMPORT))(
      s"""$CARET
         |""".stripMargin
    )

    assertElement("keyword", "import", MLFeatureValue.categorical(IMPORT))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "val", MLFeatureValue.categorical(VAL))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "var", MLFeatureValue.categorical(VAR))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "class", MLFeatureValue.categorical(CLASS))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "if", MLFeatureValue.categorical(IF))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "if", MLFeatureValue.categorical(IF))(
      s"""object X {
         |  if (true) {
         |  }
         |  else $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "else", MLFeatureValue.categorical(ELSE))(
      s"""object X {
         |  if (true) {
         |  }
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "yield", MLFeatureValue.categorical(YIELD))(
      s"""object X {
         |  for {
         |    _ <- List.empty
         |  }
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "else", MLFeatureValue.categorical(ELSE))(
      s"""object X {
         |  if (true) {
         |  }
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "try", MLFeatureValue.categorical(TRY))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("keyword", "catch", MLFeatureValue.categorical(CATCH))(
      s"""object X {
         |  try {
         |  }
         |  $CARET
         |}
         |""".stripMargin
    )
  }

  def testSymbolic(): Unit = {
    assertElement("symbolic", "+", MLFeatureValue.binary(true))(
      s"""object X {
         |  Set.empty $CARET
         |}
         |""".stripMargin
    )

    assertElement("symbolic", "--", MLFeatureValue.binary(true))(
      s"""object X {
         |  Set.empty $CARET
         |}
         |""".stripMargin
    )

    assertElement("symbolic", "contains", MLFeatureValue.binary(false))(
      s"""object X {
         |  Set.empty $CARET
         |}
         |""".stripMargin
    )
  }

  def testUnary(): Unit = {
    assertElement("unary", "unary_+", MLFeatureValue.binary(true))(
      s"""object X {
         |  1 $CARET
         |}
         |""".stripMargin
    )

    assertElement("unary", "+", MLFeatureValue.binary(false))(
      s"""object X {
         |  1 $CARET
         |}
         |""".stripMargin
    )
  }

  def testScala(): Unit = {

    assertElement("scala", "List", MLFeatureValue.binary(true))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("scala", "NoSuchMethodError", MLFeatureValue.binary(false))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )
  }

  def testJavaObjectMethod(): Unit = {

    assertElement("java_object_method", "equals", MLFeatureValue.binary(true))(
      s"""object X {
         |  1 $CARET
         |}
         |""".stripMargin
    )

    assertElement("java_object_method", "+", MLFeatureValue.binary(false))(
      s"""object X {
         |  1 $CARET
         |}
         |""".stripMargin
    )

    assertElement("java_object_method", "to", MLFeatureValue.binary(false))(
      s"""object X {
         |  1 $CARET
         |}
         |""".stripMargin
    )
  }

  def testArgumentCount(): Unit = {

    assertElement("argument_count", "Nil", MLFeatureValue.float(-1.0))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("argument_count", "f", MLFeatureValue.float(0.0))(
      s"""object X {
         |  def f(): Unit = ???
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("argument_count", "print", MLFeatureValue.float(1.0))(
      s"""object X {
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("argument_count", "f", MLFeatureValue.float(2))(
      s"""object X {
         |  val f: (Int, Int) => Double = ???
         |  $CARET
         |}
         |""".stripMargin
    )

    assertElement("argument_count", "f", MLFeatureValue.float(0))(
      s"""object X {
         |  def f(implicit int: Int): Unit = ???
         |  $CARET
         |}
         |""".stripMargin
    )
  }

  def testNameNameDist(): Unit = {

    assertElement("name_name_sim", "List", MLFeatureValue.float(-1.0))(
      s"""object X {
         |  val l = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "List", MLFeatureValue.float(1.0))(
      s"""object X {
         |  val list = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "ScalaReflectionException", MLFeatureValue.float(1.0))(
      s"""object X {
         |  var scalaReflectionException = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "ind", MLFeatureValue.float(0.6))(
      s"""object X {
         |  val ind = ???
         |  "".charAt(index = $CARET)
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "List", MLFeatureValue.float(0.5))(
      s"""object X {
         |  def byteList = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "List", MLFeatureValue.float(0.25))(
      s"""object X {
         |  type ByteListTypeName = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_name_sim", "List", MLFeatureValue.float(0.5))(
      s"""object X {
         |  def f(listname: Nothing) = ???
         |  f($CARET)
         |}
         |""".stripMargin
    )

    // TODO for some reason expectedTypeEx don't return name for non local methods
//    assertElement("name_name_sim", "requ", MLFeatureValue.float(0.5))(
//      s"""object X {
//         |  val requ = ???
//         |  require($CARET)
//         |}
//         |""".stripMargin
//    )
  }

  def testNameTypeDist(): Unit = {

    assertElement("name_type_sim", "Array", MLFeatureValue.float(-1.0))(
      s"""object X {
         |  type A = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_type_sim", "List", MLFeatureValue.float(0.25))(
      s"""object X {
         |  type ByteListTypeName = $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_type_sim", "Product12", MLFeatureValue.float(1.0))(
      s"""object X {
         |  def f(product11: $CARET)
         |}
         |""".stripMargin
    )

    assertElement("name_type_sim", "List", MLFeatureValue.float(1.0))(
      s"""object X {
         |  val list: $CARET
         |}
         |""".stripMargin
    )

    assertElement("name_type_sim", "a", MLFeatureValue.float(0.5))(
      s"""object X {
         |  val a: List[Int] = ???
         |  def f(listname: Nothing) = ???
         |  f($CARET)
         |}
         |""".stripMargin
    )
  }

  def testTypeNameDist(): Unit = {

    assertElement("type_name_sim", "Array", MLFeatureValue.float(-1.0))(
      s"""object X {
         |  val a = $CARET
         |}
         |""".stripMargin
    )

    assertElement("type_name_sim", "integral", MLFeatureValue.float(0.375))(
      s"""object X {
         |  val integral = ???
         |  val b: Int = $CARET
         |}
         |""".stripMargin
    )

    assertElement("type_name_sim", "index", MLFeatureValue.float(0.4))(
      s"""object X {
         |  val index = ???
         |  "".charAt($CARET)
         |}
         |""".stripMargin
    )

    // TODO for some reason expectedTypeEx don't return any type for overloading
//    assertElement("name_name_sim", "requ", MLFeatureValue.float(0.5))(
//      s"""object X {
//         |  val string = ???
//         |  "".indexOf($CARET)
//         |}
//         |""".stripMargin
//    )
  }

  def testTypeTypeDist(): Unit = {

    assertElement("type_type_sim", "integer", MLFeatureValue.float(-1.0))(
      s"""object X {
         |  type I = Int
         |  val integer: Int = ???
         |  val anotherInteger: I = $CARET
         |}
         |""".stripMargin
    )

    assertElement("type_type_sim", "a", MLFeatureValue.float(0.5))(
      s"""object X {
         |  val a: Option[Int] = ???
         |  var b: Int with Double = $CARET
         |}
         |""".stripMargin
    )

    assertElement("type_type_sim", "a", MLFeatureValue.float(1.0))(
      s"""object X {
         |  val a: Option[Int] = ???
         |  def f(o: Option[Int]) = ???
         |  f($CARET)
         |}
         |""".stripMargin
    )

    assertElement("type_type_sim", "a", MLFeatureValue.float(1.0))(
      s"""object X {
         |  val a: Option[Int] = ???
         |  val b: Int = $CARET
         |}
         |""".stripMargin
    )

    assertElement("type_type_sim", "true", MLFeatureValue.float(1.0))(
      s"""object X {
         |  def f(x: Boolean): Unit = ???
         |  f($CARET)
         |}
         |""".stripMargin
    )
  }

  private def assertContext(name: String, expected: MLFeatureValue)(fileText: String): Unit = {
    val elementsFeatures = computeElementsFeatures(fileText)
    AssertFeatureValues.equals(expected, elementsFeatures.head._2.get(name))
  }

  private def assertElement(name: String, element: String, expected: MLFeatureValue)(fileText: String): Unit = {
    val elementFeatures = computeElementsFeatures(fileText)
    AssertFeatureValues.equals(expected, elementFeatures(element).get(name))
  }

  private def computeElementsFeatures(fileText: String): Map[String, util.Map[String, MLFeatureValue]] = {
    class ScalaElementFeatureProviderWrapper extends ElementFeatureProvider {

      private val original = new ScalaElementFeatureProvider

      val elements = mutable.Map.empty[String, util.Map[String, MLFeatureValue]]

      override def getName: String = original.getName

      override def calculateFeatures(element: LookupElement, location: CompletionLocation, contextFeatures: ContextFeatures): util.Map[String, MLFeatureValue] = {
        val result = original.calculateFeatures(element, location, contextFeatures)

        val name = element.getObject match {
          case named: PsiNamedElement => named.name
          case _ => element.getObject.toString
        }

        elements += name -> result

        result
      }
    }

    val provider = new ScalaElementFeatureProviderWrapper
    try {
      ElementFeatureProvider.EP_NAME.addExplicitExtension(ScalaLanguage.INSTANCE, provider)

      configureFromFileText(fileText)
      changePsiAt(getEditor.getCaretModel.getOffset)
      getFixture.complete(CompletionType.BASIC, 1)

      val handler = new CodeCompletionHandlerBase(CompletionType.BASIC, false, false, true)
      handler.invokeCompletion(getProject, getEditor, 1)

      provider.elements.toMap
    }
    finally {
      ElementFeatureProvider.EP_NAME.removeExplicitExtension(ScalaLanguage.INSTANCE, provider)
    }
  }
}
