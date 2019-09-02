package org.jetbrains.plugins.scala.annotator.element

import org.jetbrains.plugins.scala.codeInspection.ScalaAnnotatorQuickFixTestBase

class ScNamingPatternAnnotatorTest extends ScalaAnnotatorQuickFixTestBase {

  override protected val description = "':' syntax in vararg pattern requires Scala 3.0"

  private val start = "<selection>"
  private val end= "</selection>"

  def testVarargPatternWithColonWithBindName(): Unit = {
    val code =
      s"""List(1, 2, 3) match {
         |  case List(first, other$start:$end _*) =>
         |}
         |"""
    checkTextHasError(code)

    testQuickFix(
      code,
      s"""List(1, 2, 3) match {
         |  case List(first, other@_*) =>
         |}
         |""",
      "Replace with '@'"
    )
  }

  def testVarargPatternWithColonWithBindWildcard(): Unit = {
    val code =
      s"""List(1, 2, 3) match {
         |  case List(first, _$start:$end _*) =>
         |}
         |"""
    checkTextHasError(code)

    testQuickFix(
      code,
      s"""List(1, 2, 3) match {
         |  case List(first, _@_*) =>
         |}
         |""",
      "Replace with '@'"
    )
  }

  def testVarargPatternWithAt(): Unit = checkTextHasNoErrors(
    s"""List(1, 2, 3) match {
       |  case List(first, other@_*) =>
       |}"""
  )
}
