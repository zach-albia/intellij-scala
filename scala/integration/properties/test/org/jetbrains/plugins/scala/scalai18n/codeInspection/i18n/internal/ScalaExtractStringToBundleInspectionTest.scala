package org.jetbrains.plugins.scala
package scalai18n
package codeInspection
package i18n
package internal

import com.intellij.codeInspection.LocalInspectionTool
import org.jetbrains.plugins.scala.codeInspection.{InspectionBundle, ScalaInspectionTestBase}

class ScalaExtractStringToBundleInspectionTest extends ScalaInspectionTestBase{
  override protected val classOfInspection: Class[_ <: LocalInspectionTool] =
    classOf[ScalaExtractStringToBundleInspection]

  override protected val description = InspectionBundle.message("internal.string.should.be.in.bundle")

  override protected def createTestText(text: String): String =
    s"""
       |object org {
       |  object jetbrains {
       |    object annotations {
       |      class Nls extends scala.annotation.StaticAnnotation
       |    }
       |  }
       |}
       |import org.jetbrains.annotations.Nls
       |
       |def toNls(@Nls arg: String): Unit = ()
       |
       |$text
       |""".stripMargin


  def test_simple_string(): Unit =
    checkTextHasError(raw""" toNls($START"blub"$END) """)

  def test_interpolated_string(): Unit =
    checkTextHasError(raw"""val v = 3; toNls(${START}s"$$v blub"$END) """)

  def test_concated_string(): Unit =
    checkTextHasError(raw""" toNls(${START}"blub" + "abc"$END) """)

  def test_string_in_parenthesis(): Unit =
    checkTextHasError(raw""" toNls( ($START"blub"$END) ) """)

  def test_string_to_named_parameter(): Unit =
    checkTextHasError(raw""" toNls( arg = $START"blub"$END) """)

  def test_string_to_overriding_parameter(): Unit =
    checkTextHasError(
      raw"""
           |trait Base { def toNls(@Nls arg: String): Unit }
           |object Impl extends Base { override def toNls(arg: String): Unit = () }
           |Impl.toNls($START"blub"$END)
           |""".stripMargin)

  def test_string_to_overriding_named_parameter(): Unit =
    checkTextHasError(
      raw"""
           |trait Base { def toNls(@Nls arg: String): Unit }
           |object Impl extends Base { override def toNls(arg: String): Unit = () }
           |Impl.toNls(arg = $START"blub"$END)
           |""".stripMargin)

  def test_string_in_block(): Unit =
    checkTextHasError(
      raw"""
           |toNls(
           |  {
           |    "not to nls"
           |    $START"blub"$END
           |  }
           |)
           |""".stripMargin)

  def test_string_in_callblock(): Unit =
    checkTextHasError(
      raw"""
           |toNls {
           |  "not to nls"
           |  $START"blub"$END
           |}
           |""".stripMargin)

  def test_string_in_callblock_witch_cases(): Unit =
    checkTextHasError(
      raw"""
           |toNls {
           |  case "not to nls" if "not to nls" =>
           |    "not to nls"
           |    $START"blub"$END
           |}
           |""".stripMargin)

  def test_string_in_infix(): Unit =
    checkTextHasError(
      raw"""
           |object X {
           |  def asInfix(@Nls str: String): Unit = ()
           |}
           |X asInfix $START"blub"$END
           |""".stripMargin)

  def test_string_in_infix_2_params(): Unit =
    checkTextHasError(
      raw"""
           |object X {
           |  def asInfix(@Nls str: String, int: Int): Unit = ()
           |}
           |X asInfix ($START"blub"$END, 1)
           |""".stripMargin)

  def test_string_in_if(): Unit =
    checkTextHasError(
      raw"""
           |toNls(if ("".toBoolean) $START"in then"$END
           |      else $START"in else"$END)
           |""".stripMargin
    )

  def test_string_in_match(): Unit =
    checkTextHasError(
      raw"""
           |toNls("string" match {
           | case false => $START"in true"$END
           | case true => $START"in false"$END
           |})
           |""".stripMargin)

  def test_string_in_typeExpr(): Unit =
    checkTextHasError(
      raw"""
           |toNls($START"blub"$END: String)
           |""".stripMargin)

  def test_in_def(): Unit =
    checkTextHasError(
      raw"""
           |@Nls
           |def test = $START"blub"$END
           |""".stripMargin)

  def test_in_annotated_val(): Unit =
    checkTextHasError(
      raw"""
           |@Nls
           |val test = $START"blub"$END
           |""".stripMargin)

  def test_in_overriding_val(): Unit =
    checkTextHasError(
      raw"""
           |trait Base { @Nls def test: String }
           |new Base { val test = $START"blub"$END }
           |""".stripMargin)

  def test_in_lazyval(): Unit =
    checkTextHasError(
      raw"""
           |@Nls
           |lazy val test = $START"blub"$END
           |""".stripMargin)

  def test_in_var(): Unit =
    checkTextHasError(
      raw"""
           |@Nls
           |var test = $START"blub"$END
           |""".stripMargin)

  def test_assign_to_var(): Unit =
    checkTextHasError(
      raw"""
           |@Nls
           |var test = null
           |test = $START"blub"$END
           |""".stripMargin)

  def test_assign_to_method(): Unit =
    checkTextHasError(
      raw"""
           |object X {
           |  def test: String = ""
           |  def test_=(@Nls arg: String): Unit = ()
           |}
           |
           |X.test = $START"blub"$END
           |""".stripMargin)
}
