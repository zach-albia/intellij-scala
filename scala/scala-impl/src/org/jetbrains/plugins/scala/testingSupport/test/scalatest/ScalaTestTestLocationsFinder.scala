package org.jetbrains.plugins.scala.testingSupport.test.scalatest

import com.intellij.openapi.module.Module
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{MethodInvocation => ScMethodInvocation, _}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.macroAnnotations.Measure
import org.scalatest.finders._

object ScalaTestTestLocationsFinder {

  type TestLocations = Seq[PsiElement]

  /** should be called in a read action  */
  @Measure
  def calculateTestLocations(definition: ScTypeDefinition, module: Module): Option[TestLocations] = {
    //Thread.sleep(5000) // uncomment to test long resolve
    val finder = ScalaTestAstTransformer.getFinder(definition, module)
    finder.flatMap(doCalculateScalaTestTestLocations(definition, _))
  }

  // NOTE 1:
  //   ScalaTestAstTransformer.getFinder is only used to determine which scalatest style is used
  //   for simplicity we now place the logic of test locations search right here
  //   when the implementation is stable we could merge this logic with org.scalatest.finders.Finder interface
  // NOTE 2: current implementation is very basic, it doesn't involve any resolve, it only analyses static names
  //   see ScalaTestConfigurationProducer.getTestClassWithTestName
  private def doCalculateScalaTestTestLocations(definition: ScTypeDefinition, finder: Finder): Option[TestLocations] = {
    val body = definition.extendsBlock.templateBody match {
      case Some(value) => value
      case None        => return None
    }
    finder match {
      case _: FunSuiteFinder    => Some(funSuiteTestLocations(body))
      case _: FlatSpecFinder    => Some(flatSpecTestLocations(body))
      case _: FunSpecFinder     => Some(funSpecTestLocations(body))
      case _: WordSpecFinder    => Some(wordSpecTestLocations(body))
      case _: FreeSpecFinder    => Some(freeSpecTestLocations(body))
      case _: PropSpecFinder    => Some(propSpecTestLocations(body))
      case _: FeatureSpecFinder => Some(featureSpecTestLocations(body))
      case _                    => None
    }
  }

  import SuiteMethodNames._

  private def funSuiteTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = false, EmptySet, FunSuiteLeaves)

  private def flatSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = true, EmptySet, FlatSpecLeaves)

  private def funSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = false, FunSpecNodes, FunSpecLeaves)

  private def wordSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = true, WordSpecNodes, WordSpecLeaves)

  private def freeSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = true, FreeSpecNodes, FreeSpecLeaves)

  private def propSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = false, EmptySet, PropSpecLeaves)

  private def featureSpecTestLocations(body: ScTemplateBody): TestLocations =
    collectTestLocations(body, infixStyle = false, FeatureSpecNodes, FeatureSpecLeaves)

  private def collectTestLocations(
    body: ScTemplateBody,
    infixStyle: Boolean,
    intermediateMethodNames: Set[String],
    leafMethodNames: Set[String]
  ): TestLocations = {

    def inner(expressions: Seq[ScExpression]): Seq[ScReferenceExpression] = expressions.flatMap { expr =>
      val (methodCall, target) = expr match {
        case call: ScMethodInvocation if infixStyle => (call, call.getInvokedExpr)
        case call: ScMethodCall                     => (call, call.deepestInvokedExpr)
        case _                                      => return Seq.empty
      }

      target match {
        case ref: ScReferenceExpression =>
          if (intermediateMethodNames.contains(ref.refName)) {
            val childExpressions = methodCall.argumentExpressions.collect { case block: ScBlockExpr => block.exprs }
            Seq(ref) ++ inner(childExpressions.flatten)
          } else if (leafMethodNames.contains(ref.refName)) {
            Seq(ref)
          } else {
            Seq.empty
          }
        case _ =>
          Seq.empty
      }
    }

    val constructorExpressions = body.exprs
    inner(constructorExpressions)
  }

  private object SuiteMethodNames {

    val EmptySet: Set[String] = Set()

    val FunSuiteLeaves    = Set("test")
    val FlatSpecLeaves    = Set("in", "of")
    val PropSpecLeaves    = Set("property")

    val FunSpecNodes      = Set("describe")
    val FunSpecLeaves     = Set("it", "they")

    val WordSpecNodes     = Set("when", "should", "must", "can", "which")
    val WordSpecLeaves    = Set("in")

    val FreeSpecNodes     = Set("-")
    val FreeSpecLeaves    = Set("in")

    val FeatureSpecNodes  = Set("feature")
    val FeatureSpecLeaves = Set("scenario")
  }
}
