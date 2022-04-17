import scala.annotation.tailrec

1 + 3

def abs(x: Double) = if (x < 0) -x else x

def sqrtNewton(x: Double) = {
  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.1e-10

  def improve(guess: Double) =
    (guess + x / guess) / 2

  @tailrec
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrtNewton(2)
sqrtNewton(4)
sqrtNewton(0.1e-20)
sqrtNewton(1.0e20)
sqrtNewton(1.0e50)
