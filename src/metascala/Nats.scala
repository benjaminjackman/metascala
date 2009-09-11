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

    def toInt : Int
  }
  
  final class _0 extends Nat {
    type Pre = _0
    type Is0 = True
    type Add[N <: Nat] = N
    type Accept[N <: NatVisitor] = N#Visit0

    def toInt = 0
  }
  
  final case class Succ[P <: Nat](toInt : Int) extends Nat {
    type Pre = P
    type Is0 = False
    type Add[N <: Nat] = Succ[P#Add[N]]
    type Accept[N <: NatVisitor] = N#VisitSucc[P]

    def +[N <: Nat](n : N) : Add[N] = Succ[P#Add[N]](toInt + n.toInt)
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
  val _1 = new _1(1)
  val _2 = new _2(2)
  val _3 = new _3(3)
  val _4 = new _4(4)
  val _5 = new _5(5)
  val _6 = new _6(6)
  val _7 = new _7(7)
  val _8 = new _8(8)
  val _9 = new _9(9)
  val _10 = new _10(10)

  implicit val _0ToInt = TypeToValue[_0, Int](0)
  implicit def succToInt[P <: Nat](implicit v : TypeToValue[P, Int]) = TypeToValue[Succ[P], Int](1 + v.value)

  def toInt[T <: Nat](v : T)(implicit fn : TypeToValue[T, Int]) = fn()
}
