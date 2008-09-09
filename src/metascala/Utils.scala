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
  
  trait Addable {
    type AddType <: Addable
    type Add[T <: AddType] <: AddType
  }
  
  type +[A1 <: Addable, A2 <: A1#AddType] = A1#Add[A2]
  
  trait Subtractable {
    type SubType <: Subtractable
    type Sub[T <: SubType] <: SubType
  }
  
  type -[S1 <: Subtractable, S2 <: S1#SubType] = S1#Sub[S2]
  
  def value[T] : T = null.asInstanceOf[T]
}
