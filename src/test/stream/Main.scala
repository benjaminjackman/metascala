package stream

object Main {
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

  def sumLoop(a : Array[Int]) = {
    var i = 0
    var r = 0

    while (i < a.length) {
      r += a(i)
      i += 1
    }

    r
  }

  def map(a : Array[Int]) = {
    var i = 0
    var b = new Array[Int](a.length)

    while (i < a.length) {
      b(i) = a(i) * 3 + 7
      i += 1
    }

    b
  }

  def filter(a : Array[Int]) = {
    var b = new Array[Int](a.length)
    var j = 0
    var i = 0

    while (i < a.length) {
      val v = a(i)

      if ((v % 10) == 0) {
        b(j) = v
        j += 1
      }

      i += 1
    }

    var c = new Array[Int](j)
    System.arraycopy(b, 0, c, 0, c.length)
    c
  }

  // Benchmark function
  def mapFilterSumLoopIntermediate(a : Array[Int]) = {
    sumLoop(filter(map(a)))
  }

  def scalaArraySum(a : Array[Int]) = a.map(i => i * 3 + 7).filter(i => (i % 10) == 0).foldLeft(0)(_ + _)

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
    println("Loop: " + bench(mapFilterSumLoop))
    println("Loop with intermediate arrays: " + bench(mapFilterSumLoopIntermediate))
    println("Scala library: " + bench(scalaArraySum))
    println("Specialized iterators: " + bench(SpecializedIterators.mapFilterSum))
  }
}
