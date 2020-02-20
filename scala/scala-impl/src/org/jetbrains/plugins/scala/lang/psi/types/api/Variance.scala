package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, Stop}

/** Copied almost verbatim from [[scala.reflect.internal.Variance]]
  *
  * Variances form a lattice:
  *
  *            - Covariant -
  *           /             \
  *  Invariant               Bivariant
  *           \             /
  *            Contravariant
  *
  *  The variance of a symbol within a type is calculated based on variance
  *  annotations, e.g. +A or -A, and the positions of the types in which the
  *  symbol appears. The actual mechanics are beyond the scope of this
  *  comment, but the essential operations on a Variance are:
  *
  *  '&'  - like bitwise AND. Unless all inputs have compatible variance,
  *  folding them across & will be invariant.
  *  '*'  - like multiplication across { -1, 0, 1 } with contravariance as -1.
  *  flip - if contravariant or covariant, flip to the other; otherwise leave unchanged.
  *  cut  - if bivariant, remain bivariant; otherwise become invariant.
  *
  *  There is an important distinction between "isPositive" and "isCovariant".
  *  The former is true for both Covariant and Bivariant, but the latter is true
  *  only for Covariant.
  */
final class Variance private (val flags: Int) extends AnyVal {
  def isBivariant: Boolean = flags == 2
  def isCovariant: Boolean = flags == 1    // excludes bivariant
  def isInvariant: Boolean = flags == 0
  def isContravariant: Boolean = flags == -1   // excludes bivariant
  def isPositive: Boolean = flags > 0     // covariant or bivariant

  def &(other: Variance): Variance =
    if (this == other) this
    else if (this.isBivariant) other
    else if (other.isBivariant) this
    else Invariant

  def *(other: Variance): Variance =
    if (other.isPositive) this
    else if (other.isContravariant) -this
    else this.cut

  def flip: Variance = if (isCovariant) Contravariant else if (isContravariant) Covariant else this

  def unary_- : Variance = flip

  def inverse(b: Boolean): Variance = if (b) flip else this

  def sign: Variance = if (isContravariant) Contravariant else Covariant

  /** Map everything below bivariant to invariant. */
  def cut: Variance = if (isBivariant) this else Invariant

  /** The symbolic annotation used to indicate the given kind of variance. */
  def symbolicString: String =
    if (isCovariant) "+"
    else if (isContravariant) "-"
    else ""

  def name: String =
    if (isContravariant) ScalaBundle.message("variance.contravariant")
    else if (isCovariant) ScalaBundle.message("variance.covariant")
    else if (isInvariant) ScalaBundle.message("variance.invariant")
    else ScalaBundle.message("variance.bivariant")

  override def toString: String =
    if (isBivariant) "" else name // noisy to print bivariant on everything without type parameters
}

object Variance {
  val Bivariant     = new Variance(2)
  val Covariant     = new Variance(1)
  val Contravariant = new Variance(-1)
  val Invariant     = new Variance(0)
}