package stream

object SpecializedIterators {
  // Specialized Function1
  trait Fn1[@specialized I, @specialized O] {
    def apply(a : I) : O
  }

  // Specialized Function2
  trait Fn2[@specialized I1, @specialized I2, @specialized O] {
    def apply(a1 : I1, a2 : I2) : O
  }

  // Specialized iterator
  trait SIterator[@specialized T] {
    def hasMore : Boolean
    def current : T
    def next()
  }

  class IntArrayIterator(a : Array[Int], var index : Int, endIndex : Int) extends SIterator[Int] {
    def next() = index += 1
    def current = a(index)
    def hasMore = index < endIndex
  }

  // Optimally this would be: class FilterIterator[@specialized T](iter : SIterator[T], pred : Fn1[T, Boolean]) extends SIterator[T]
  class FilterIterator(iter : SIterator[Int], pred : Fn1[Int, Boolean]) extends SIterator[Int] {
    def hasMore = iter.hasMore

    def next() = {
      iter.next()
      findNext()
    }

    def findNext() = {
      while (iter.hasMore && !pred(iter.current))
        iter.next()
    }

    def current = iter.current

    findNext()
  }

  // Optimally this would be: class MapIterator[@specialized U][@specialized T](iter : SIterator[T], fn : Fn1[T, U]) extends SIterator[U]
  class MapIterator(iter : SIterator[Int], fn : Fn1[Int, Int]) extends SIterator[Int] {
    def next() = iter.next()
    def current = fn(iter.current)
    def hasMore = iter.hasMore
  }

  def fold[@specialized T, @specialized U] (iter : SIterator[T], fn : Fn2[U, T, U], v : U) = {
    var r = v

    while (iter.hasMore) {
      r = fn(r, iter.current)
      iter.next()
    }

    r
  }

  def mapFilterSum(a : Array[Int]) = {
    val filter = new Fn1[Int, Boolean] {def apply(a : Int) = (a % 10) == 0}
    val map = new Fn1[Int, Int] {def apply(a : Int) = a * 3 + 7}
    val s = new FilterIterator(new MapIterator(new IntArrayIterator(a, 0, a.length), map), filter)
    fold(s, new Fn2[Int, Int, Int] {def apply(a1 : Int, a2 : Int) = a1 + a2}, 0)
  }

}
