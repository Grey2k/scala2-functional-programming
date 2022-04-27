package week3

import scala.annotation.tailrec

class Rational(x: Int, y: Int) {
  require(y > 0, s"denominator must be positive, given $y")

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def abs(x: Int): Int         = if (x < 0) -x else x
  private val g                        = abs(gcd(x, y))

  val numerator: Int   = x / g
  val denominator: Int = y / g

  override def toString = s"$numerator/$denominator"

  def less(that: Rational): Boolean =
    numerator * that.denominator < that.numerator * denominator

  def <(that: Rational): Boolean = this.less(that)

  def >(that: Rational): Boolean = !this.less(that)

  def max(that: Rational): Rational = if (this.less(that)) that else this

  def add(that: Rational): Rational =
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )

  def neg = new Rational(-this.numerator, this.denominator)

  def unary_-(): Rational = this.neg

  def sub(that: Rational): Rational = add(that.neg)

  def mul(that: Rational) =
    new Rational(
      numerator * that.numerator,
      denominator * that.denominator
    )

  def +(that: Rational): Rational = this.add(that)

  def -(that: Rational): Rational = this + -that

  def *(that: Rational): Rational = this.mul(that)
}
