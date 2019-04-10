package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScLiteralType.Kind
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.junit.Assert

/**
 * Pavel Fatin
 */

class ScLiteralTest extends SimpleTestCase {
  def testNullLiteral() {
    assertTypeIs("null", Null)
  }

  def testIntLiteral() {
    assertTypeIs("1", ScLiteralType(1, Kind.Int))
  }

  def testLongLiteral() {
    assertTypeIs("1l", ScLiteralType(1L, Kind.Long))
    assertTypeIs("1L", ScLiteralType(1L, Kind.Long))
  }

  def testFloatLiteral() {
    assertTypeIs("1f", ScLiteralType(1f, Kind.Float))
    assertTypeIs("1F", ScLiteralType(1f, Kind.Float))
  }

  def testDoubleLiteral() {
    assertTypeIs("1d", ScLiteralType(1d, Kind.Double))
    assertTypeIs("1D", ScLiteralType(1d, Kind.Double))
  }

  def testCharLiteral() {
    assertTypeIs("'c'", ScLiteralType('c', Kind.Char))
  }

  def testBooleanLiteral() {
    assertTypeIs("true", ScLiteralType(true, Kind.Boolean))
    assertTypeIs("false", ScLiteralType(false, Kind.Boolean))
  }

  private def assertTypeIs(code: String, expectation: ScType) {
    val exp = code.parse[ScExpression]
    val actual = exp.`type`().get
    Assert.assertEquals(expectation, actual)
  }
}