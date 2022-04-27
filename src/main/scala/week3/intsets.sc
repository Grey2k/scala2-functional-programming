import week3.Rational

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this

  override def toString: String = s"{$left-$elem-$right}"

  override def union(other: IntSet): IntSet =
    left
      .union(right)
      .union(other)
      .incl(elem)
}

/** Empty set
  */
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet      = new NonEmpty(x, Empty, Empty)

  override def toString: String = "."

  override def union(other: IntSet): IntSet = other
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1.incl(4)

t2.union(new NonEmpty(9, Empty, Empty))

new Rational(1, 4)
