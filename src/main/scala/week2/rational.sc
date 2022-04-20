import scala.annotation.tailrec

class Rational(x: Int, y: Int) {
  require(y > 0, s"denominator must be positive, given $y")

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def abs(x: Int): Int         = if (x < 0) -x else x
  private val g                        = abs(gcd(x, y))

  val numerator   = x / g
  val denominator = y / g

  override def toString = s"$numerator/$denominator"

  def less(that: Rational): Boolean =
    numerator * that.denominator < that.numerator * denominator

  def <(that: Rational) = this.less(that)

  def >(that: Rational) = !this.less(that)

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational): Rational =
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )

  def neg = new Rational(-this.numerator, this.denominator)

  def unary_-(): Rational = this.neg

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(
      numerator * that.numerator,
      denominator * that.denominator
    )

  def +(that: Rational) = this.add(that)

  def -(that: Rational) = this + -that

  def *(that: Rational) = this.mul(that)
}

/*
 *    Addition
 *
 *     a     c      ad + bc
 *    --- + ---  =  ------
 *     b     d        bd
 *
 */
def addRational(r: Rational, s: Rational): Rational =
  new Rational(
    r.numerator * s.denominator + s.numerator * r.denominator,
    r.denominator * s.denominator
  )

def makeString(r: Rational): String =
  s"${r.numerator}/${r.denominator}"

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.numerator
x.denominator

y.numerator
y.denominator

makeString(x.add(new Rational(3, 4)))

println(makeString(x))

makeString(addRational(new Rational(1, 4), new Rational(3, 5)))

x.sub(y).sub(z)
x.add(y).mul(z)

x > y
x < z

// val strange = new Rational(1, 0)

// Infix
x sub y sub z
x add y mul z

// Symbolic
x - y - z
x + y * z
