import scala.annotation.tailrec

def factorial(i: Int) = {
  @tailrec
  def factIter(i: Int, accum: Int): Int = {
    if (i == 0) accum
    else factIter(i - 1, i * accum)
  }

  factIter(i, 1)
}

factorial(3)
assert(factorial(3) == 6)
