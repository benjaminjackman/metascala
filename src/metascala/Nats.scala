package metascala

object Nats {
  import Utils._
  import Booleans._
  
  trait NatVisitor extends TypeVisitor {
    type Visit0 <: ResultType
    type VisitSucc[Pre <: Nat] <: ResultType
  }
  
  sealed trait Nat extends Addable {
    type Pre <: Nat
    type Is0 <: Bool
    type Add[T <: Nat] <: Nat
    type AddType = Nat
    type Accept[N <: NatVisitor] <: N#ResultType
    type Eq[N <: Nat] <: Bool
  }
  
  final class _0 extends Nat {
    type Pre = _0
    type Is0 = True
    type Add[N <: Nat] = N
    type Accept[N <: NatVisitor] = N#Visit0
  }
  
  final class Succ[P <: Nat] extends Nat {
    type Pre = P
    type Is0 = False
    type Add[N <: Nat] = Succ[P#Add[N]]
    type Accept[N <: NatVisitor] = N#VisitSucc[P]
  }
  
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  
  val _0 = new _0
  val _1 = new _1
  val _2 = new _2
  val _3 = new _3
  val _4 = new _4
  val _5 = new _5
  val _6 = new _6
  val _7 = new _7
  val _8 = new _8
  val _9 = new _9
  val _10 = new _10

  implicit val _0ToInt = TypeToValue[_0, Int](0)
  implicit def succToInt[P <: Nat](implicit v : TypeToValue[P, Int]) = TypeToValue[Succ[P], Int](1 + v.value)
}
