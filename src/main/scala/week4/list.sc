import scala.annotation.tailrec

trait IntList {}

class Cons(val head: Int, val tail: IntList) extends IntList

class Nil extends IntList

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing    = throw new NoSuchElementException("Nil.head")
  def tail: Nothing    = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton[Boolean](true)
singleton(1)
singleton(true)

def getNth[T](index: Int, list: List[T]): T = {
  @tailrec
  def find(cursor: List[T], p: Int): T = {
    if (cursor.isEmpty)
      throw new IndexOutOfBoundsException(
        s"Index '$index' out of range '0-${p - 1}'"
      )
    if (p == index) cursor.head
    else find(cursor.tail, p + 1)
  }

  find(list, 0)
}

@tailrec
def nth[T](index: Int, list: List[T]): T = {
  if (list.isEmpty)
    throw new IndexOutOfBoundsException(
      s"Index '$index' out of range"
    )
  if (index == 0) list.head
  else nth(index - 1, list.tail)
}

val list = new Cons(
  1,
  new Cons(2, new Cons(3, new Nil[Int]()))
)

getNth(0, list)
getNth(2, list)
// getNth(3, list)

nth(0, list)
nth(2, list)
nth(3, list)
