class Rational(x: Int, y: Int) {
  def numerator   = x
  def denominator = y

  override def toString = s"$numerator/$denominator"

  def add(that: Rational): Rational =
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )

  def neg = new Rational(-numerator, denominator)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(
      numerator * that.numerator,
      denominator * that.denominator
    )
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

makeString(x.add(new Rational(3, 4)))

println(makeString(x))

makeString(addRational(new Rational(1, 4), new Rational(3, 5)))

x.sub(y).sub(z)
x.add(y).mul(z)
