package stream

import stream.Fns.{IntIntIntFn, IntBoolFn, IntIntFn}

object Iterators {
  trait IntIterator {
    def empty : Boolean
    def current : Int
    def next()
  }

  class IntFilterStream(stream : IntIterator, pred : IntBoolFn) extends IntIterator {
    def current = stream.current
    def empty = stream.empty

    def next() {
      stream.next()
      findNext()
    }

    def findNext() {
      while (!stream.empty && !pred(current))
        stream.next()
    }

    findNext()
  }

  class IntMapStream(stream : IntIterator, fn : IntIntFn) extends IntIterator {
    def next() = stream.next()
    def current = fn(stream.current)
    def empty = stream.empty
  }

  class IntArrayStream(a : Array[Int], var index : Int, endIndex : Int) extends IntIterator {
    def next() = index += 1
    def current = a(index)
    def empty = index >= endIndex
  }

  def fold(s : IntIterator, fn : IntIntIntFn, v : Int) = {
    var r = v

    while (!s.empty) {
      r = fn(r, s.current)
      s.next()
    }

    r
  }

  def sum(s : IntIterator) : Int = {
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
