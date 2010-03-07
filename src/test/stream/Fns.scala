package stream

object Fns {
  trait IntIntFn {
    def apply(a : Int) : Int
  }

  trait IntIntIntFn {
    def apply(a1 : Int, a2 : Int) : Int
  }

  trait IntBoolFn {
    def apply(a : Int) : Boolean
  }
}
