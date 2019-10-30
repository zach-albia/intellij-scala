package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.{ScalaVersion, Scala_2_13}
import org.jetbrains.plugins.scala.failed.typeInference.ImplicitParametersTestBase

class ImplicitParametersTest extends ImplicitParametersTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= Scala_2_13

  def testSCL15862(): Unit = checkNoImplicitParameterProblems(
    s"""
       |class ImplicitDep()
       |class X {
       |  def callWithImplicitParam(implicit a: ImplicitDep): String = "test"
       |}
       |class TestA(x: X, private implicit val dep: ImplicitDep) {
       |  ${START}x.callWithImplicitParam$END
       |}
       |""".stripMargin
  )

  def testSCL16246(): Unit = checkNoImplicitParameterProblems(
    s"""
       |import scala.collection.Factory
       |
       |case class Test(aa: String)
       |
       |case object Test extends App {
       |
       |  ${START}test()$END
       |
       |  def test()(implicit factory: Factory[Test, List[Test]]): Unit = {
       |    println(factory.getClass)
       |  }
       |}
       |""".stripMargin
  )
}
