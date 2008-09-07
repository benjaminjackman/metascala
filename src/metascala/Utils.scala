package metascala

object Utils {
  trait Equal[T1 >: T2 <: T2, T2]

  class Fn1Wrapper[T1, R](fn : T1 => R) {
    def apply(a1 : T1) = fn(a1)
  }
  
  class Fn2Wrapper[T1, T2, R](fn : (T1, T2) => R) {
    def apply(a1 : T1, a2 : T2) = fn(a1, a2)
  }
  
  case class TypeToValue[T, VT](value : VT) {
    def apply() = value
  }
  
  def to[T, VT](implicit fn : TypeToValue[T, VT]) = fn()
  
  trait TypeVisitor {
    type ResultType
  }
  
  trait Visitable[V <: TypeVisitor] {
    type Accept[V2 <: V] <: V2#ResultType
  }
}
