package metascala

object Units {
  import Integers._
  
  trait Unit {
    type M <: MInt
    type KG <: MInt
    type S <: MInt
    type A <: MInt
    type K <: MInt
    type Mol <: MInt
    type CD <: MInt
  }
  
  final class TUnit[_M <: MInt, _KG <: MInt, _S <: MInt, _A <: MInt, _K <: MInt, _Mol <: MInt, _CD <: MInt] {
    type M = _M
    type KG = _KG
    type S = _S
    type A = _A
    type K = _K
    type Mol = _Mol
    type CD = _CD    
  }
  
  case class Measure[M <: MInt, KG <: MInt, S <: MInt, A <: MInt, K <: MInt, Mol <: MInt, CD <: MInt](value : Double) {
    type This = Measure[M, KG, S, A, K, Mol, CD]
    def +(m : This) = Measure[M, KG, S, A, K, Mol, CD](value + m.value)
    def *[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](m : Measure[M2, KG2, S2, A2, K2, Mol2, CD2]) = Measure[M + M2, KG + KG2, S + S2, A + A2, K + K2, Mol + Mol2, CD + CD2](value * m.value)
    def /[M2 <: MInt, KG2 <: MInt, S2 <: MInt, A2 <: MInt, K2 <: MInt, Mol2 <: MInt, CD2 <: MInt](m : Measure[M2, KG2, S2, A2, K2, Mol2, CD2]) = Measure[M - M2, KG - KG2, S - S2, A - A2, K - K2, Mol - Mol2, CD - CD2](value * m.value)
  }
  
  implicit def measure(v : Double) = Measure[_0, _0, _0, _0, _0, _0, _0](v)
  
  val m = Measure[_1, _0, _0, _0, _0, _0, _0](1)
  val kg = Measure[_0, _1, _0, _0, _0, _0, _0](1)
  val s = Measure[_0, _0, _1, _0, _0, _0, _0](1)
  val a = Measure[_0, _0, _0, _1, _0, _0, _0](1)
  val k = Measure[_0, _0, _0, _0, _1, _0, _0](1)
  val mol = Measure[_0, _0, _0, _0, _0, _1, _0](1)
  val cd = Measure[_0, _0, _0, _0, _0, _0, _1](1)
  
  type Length = Measure[_1, _0, _0, _0, _0, _0, _0]
  type Mass = Measure[_0, _1, _0, _0, _0, _0, _0]
  type Time = Measure[_0, _0, _1, _0, _0, _0, _0]
  type Currency = Measure[_0, _0, _0, _1, _0, _0, _0]
  type Temperature = Measure[_0, _0, _0, _0, _1, _0, _0]
  type Mol = Measure[_0, _0, _0, _0, _0, _1, _0]
  type LuminousIntensity = Measure[_0, _0, _0, _0, _0, _0, _1]
  
  type Area = Measure[_2, _0, _0, _0, _0, _0, _0] 
  type Volume = Measure[_3, _0, _0, _0, _0, _0, _0] 
  type Speed = Measure[_1, _0, _1#Neg, _0, _0, _0, _0] 
  type Acceleration = Measure[_1, _0, _2#Neg, _0, _0, _0, _0] 
}
