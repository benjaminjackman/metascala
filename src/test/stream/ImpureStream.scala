package stream

import stream.Fns._

object ImpureStreams {
  trait IntStream {
    def empty : Boolean
    def currentValid : Boolean
    def current : Int
    def next()
  }

  class IntFilterStream(stream : IntStream, pred : IntBoolFn) extends IntStream {
    def next() = stream.next()
    def current = stream.current
    def currentValid = pred(current) && stream.currentValid
    def empty = stream.empty
  }

  class IntMapStream(stream : IntStream, fn : IntIntFn) extends IntStream {
    def next() = stream.next()
    def current = fn(stream.current)
    def currentValid = stream.currentValid
    def empty = stream.empty
  }

  class IntArrayStream(a : Array[Int], var index : Int, endIndex : Int) extends IntStream {
    def next() = index += 1
    def current = a(index)
    def currentValid = true
    def empty = index >= endIndex
  }

  def fold(s : IntStream, fn : IntIntIntFn, v : Int) = {
    var r = v

    while (!s.empty) {
      if (s.currentValid)
        r = fn(r, s.current)

      s.next()
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
