package metascala


object HArrays {
  import Nats._
  import TLists._
  import HCollections._

  final class HArray[L <: TList](private val value : Array[Any]) extends Tuple[L] {
    type Types = L
    type This[L <: TList] = HArray[L]
    type Length = Types#Length

    def ::[T](v : T) = {
      val a = new Array[Any](value.length + 1)
      a(0) = v
      Array.copy(value, 0, a, 1, value.length)
      new HArray[T :: L](a)
    }

    def :::[L2 <: TList](l : HArray[L2]) = {
      val a = new Array[Any](value.length + l.value.length)
      Array.copy(l.value, 0, a, 0, l.value.length)
      Array.copy(value, 0, a, l.value.length, value.length)
      new HArray[L2#Append[L]](a)
    }

    def apply[N <: Nat](n : N) : L#Nth[N] = value(n.toInt).asInstanceOf[L#Nth[N]]

    def reverse = {
      val a = new Array[Any](value.length)

      for (i <- 0 until value.length)
        a(i) = value(value.length - 1 - i)

      new HArray[L#ReverseAppend[TNil]](a)
    }

    def removeNth[N <: Nat](n : N) = {
      val a = new Array[Any](value.length - 1)
      val i = n.toInt
      Array.copy(value, 0, a, 0, i)
      Array.copy(value, i + 1, a, i, value.length - 1 - i)
      new HArray[L#RemoveNth[N]](a)
    }

    def insert[N <: Nat, E](n : N, elem : E) = {
      val a = new Array[Any](value.length + 1)
      val i = n.toInt
      Array.copy(value, 0, a, 0, i)
      a(i) = elem
      Array.copy(value, i, a, i + 1, value.length - i)
      new HArray[L#Insert[N, E]](a)
    }

    override def equals(o : Any) = o match {
      case ha : HArray[_] => HArray.equal(value, ha.value)
      case _ => false
    }

    override def hashCode = HArray.hashCode(value)

//    def replaceSameType[N <: Nat, E](n : N, elem : E) = null
//    def getByType[N <: Nat, E](implicit fn : GetByType[N, E]) : E = fn(this)
  }

  val HArrayNil = new HArray[TNil](new Array[Any](0))

  object HArray {
    private def createArray(elems : Any*) = elems toArray

    private def equal[T](a1 : Array[T], a2 : Array[T]) : Boolean = {
      if (a1.length != a2.length)
        return false
      else {
        var i = 0

        while (i < a1.length) {
          if (a1(i) != a2(i))
            return false

          i += 1
        }

        return true
      }
    }

    private def hashCode[T](a : Array[T]) : Int = {
      var i = 0
      var h = 0

      while (i < a.length) {
        h = 37 * h + a(i).hashCode
        i += 1
      }

      return h
    }

    def apply[T1](v1 : T1) : HArray[T1 :: TNil] =
      new HArray[T1 :: TNil](createArray(v1))

    def apply[T1, T2](v1 : T1, v2 : T2) : HArray[T1 :: T2 :: TNil] =
      new HArray[T1 :: T2 :: TNil](createArray(v1, v2))

    def apply[T1, T2, T3](v1 : T1, v2 : T2, v3 : T3) : HArray[T1 :: T2 :: T3 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: TNil](createArray(v1, v2, v3))

    def apply[T1, T2, T3, T4](v1 : T1, v2 : T2, v3 : T3, v4 : T4) : HArray[T1 :: T2 :: T3 :: T4 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: T4 :: TNil](createArray(v1, v2, v3, v4))

    def apply[T1, T2, T3, T4, T5](v1 : T1, v2 : T2, v3 : T3, v4 : T4, v5 : T5) : HArray[T1 :: T2 :: T3 :: T4 :: T5 :: TNil] =
      new HArray[T1 :: T2 :: T3 :: T4 :: T5 :: TNil](createArray(v1, v2, v3, v4, v5))
  }
}
