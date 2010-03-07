package stream

import stream.Fns._

object PureStreams {
  trait IntStream {
    def empty : Boolean
    def current : Int
    def next : IntStream
    def currentValid : Boolean
  }

  class IntFilterStream(stream : IntStream, pred : IntBoolFn) extends IntStream {
    def next = new IntFilterStream(stream.next, pred)
    def current = stream.current
    def currentValid = pred(current) && stream.currentValid
    def empty = stream.empty
  }

  class IntMapStream(stream : IntStream, fn : IntIntFn) extends IntStream {
    def next = new IntMapStream(stream.next, fn)
    def current = fn(stream.current)
    def currentValid = stream.currentValid
    def empty = stream.empty
  }

  class IntArrayStream(a : Array[Int], var index : Int, endIndex : Int) extends IntStream {
    def next = new IntArrayStream(a, index + 1, endIndex)
    def current = a(index)
    def currentValid = true
    def empty = index >= endIndex
  }

  def fold(s : IntStream, fn : IntIntIntFn, v : Int) = {
    var r = v
    var s2 = s

    while (!s2.empty) {
      if (s2.currentValid)
        r = fn(r, s2.current)

      s2 = s2.next
    }

    r
  }

  def sum(s : IntStream) : Int = {
    fold(s, new IntIntIntFn { def apply(a1 : Int, a2 : Int) = a1 + a2 }, 0)
  }

  def sum(a : Array[Int]) : Int = {
    sum(new IntArrayStream(a, 0, a.length))
  }

  def filterSum(a : Array[Int]) = {
    val s = new IntFilterStream(new IntArrayStream(a, 0, a.length), new IntBoolFn { def apply(a : Int) = (a % 37) == 0 })
    sum(s)
  }

  def mapFilterSum(a : Array[Int]) = {
    val s = new IntMapStream(new IntFilterStream(new IntArrayStream(a, 0, a.length), new IntBoolFn { def apply(a : Int) = (a % 37) == 0 }), new IntIntFn { def apply(a : Int) = a * 3 + 7 })
    sum(s)
  }

}
