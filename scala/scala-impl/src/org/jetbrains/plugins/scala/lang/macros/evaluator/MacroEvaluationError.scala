package org.jetbrains.plugins.scala.lang.macros.evaluator

/**
 * Denotes an IDEA-level macro expansion error, either due to
 * a missing macro implementation or an actual failed expansion (e.g. missing implicits).
 */
sealed trait MacroEvaluationError

object MacroEvaluationError {
  case object NoRuleDefined    extends MacroEvaluationError
  case object MacroCheckFailed extends MacroEvaluationError
}
