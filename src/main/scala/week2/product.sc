import scala.annotation.tailrec

def product(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def mul(a: Int, accum: Int): Int =
    if (a > b) accum
    else mul(a + 1, accum * f(a))

  mul(a, 1)
}

def fact(n: Int) = product(x => x)(1, n)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(
    a: Int,
    b: Int
): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def mapReduceProduct(f: Int => Int)(a: Int, b: Int) =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

product(x => x * x)(3, 7)
fact(5)

def factMR(n: Int) = mapReduceProduct(x => x)(1, n)

mapReduceProduct(x => x * x)(3, 7)
factMR(5)
