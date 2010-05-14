package stream

object SpecializedIterators2 {
  // Specialized iterator
  trait SIterator[@specialized T] {
    def hasNext : Boolean
    def next() : T
    def filter(pred : T => Boolean, dummy : T) = new FilterIterator[T](this, pred)
    def map[@specialized U](fn : T => U, dummy : T, dummy2 : U) = new MapIterator[T, U](this, fn)
  }

  final class IntArrayIterator(a : Array[Int], var index : Int, endIndex : Int) extends SIterator[Int] {
    def next() = {val r = a(index); index += 1; r}
    def hasNext = index < endIndex
  }

  final class ArrayIterator[@specialized T](a : Array[T], var index : Int, endIndex : Int) extends SIterator[T] {
    def next() = {val r = a(index); index += 1; r}
    def hasNext = index < endIndex
  }

  final class FilterIterator[@specialized T](iter : SIterator[T], pred : T => Boolean) extends SIterator[T] {
    private var hasElem = false
    private var elem : T = findNext()

    def hasNext = hasElem

    def next() = {
      val r = elem
      findNext()
      r
    }

    def findNext() : T = {
      while (iter.hasNext) {
        elem = iter.next()

        if (pred(elem)) {
          hasElem = true
          return elem
        }
      }

      hasElem = false
      elem
    }
  }

  final class MapIterator[@specialized T, @specialized U](iter : SIterator[T], fn : T => U) extends SIterator[U] {
    def next() = fn(iter.next())
    def hasNext = iter.hasNext
  }

  def fold[@specialized T, @specialized U](iter : SIterator[T], fn : (U, T) => U, v : U, dummy : T) = {
    var r = v

    while (iter.hasNext) {
      r = fn(r, iter.next())
    }

    r
  }

  def mapFilterSum(a : Array[Int]) = {
    val ai = new ArrayIterator(a, 0, a.length)
    val s = new FilterIterator[Int](new MapIterator[Int, Int](ai, _ * 3 + 7), _ % 10 == 0)
    fold[Int, Int](s, _ + _, 0, 0)
  }

  def mapFilterSum2(a : Array[Int]) = {
    val ai = new ArrayIterator(a, 0, a.length)
    val s = ai.map(_ * 3 + 7, 0, 0).filter(_ % 10 == 0, 0)  // Doesn't specialize properly
    fold[Int, Int](s, _ + _, 0, 0)
  }

  def mapFilterSumLoop(a : Array[Int]) = {
    var i = 0
    var r = 0

    while (i < a.length) {
      val v = a(i) * 3 + 7

      if ((v % 10) == 0)
        r += v

      i += 1
    }

    r
  }

  def createArray(count : Int) = {
    var a = new Array[Int](count)

    for (i <- 1 until a.length)
      a(i) = a(i - 1) * 13 + 1947

    a
  }

  // Benchmark function
  def bench(proc : Array[Int] => Int) = {
    var m = java.lang.Long.MAX_VALUE
    var a = createArray(1000000)
    var r = 0

    for (i <- 0 to 200) {
      var t = System.nanoTime
      r = proc(a)
      m = Math.min(m, System.nanoTime - t)
    }

    (m / 1000, r)
  }

  def main(args : Array[String]) {
    println("Java version: " + System.getProperty("java.version"))
    println("Loop: " + bench(mapFilterSumLoop))
    println("Specialized iterators: " + bench(mapFilterSum))
  }
}
