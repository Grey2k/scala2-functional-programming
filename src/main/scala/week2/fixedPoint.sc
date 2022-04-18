import scala.annotation.tailrec
import java.lang.Math

val tolerance = 0.1e-10

def abs(x: Double) = if (x < 0) -x else x

def isCloseEnough(x: Double, y: Double): Boolean =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  @tailrec
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1 + x / 2)(1)

def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)

sqrt(2)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def stableSqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

stableSqrt(2)
