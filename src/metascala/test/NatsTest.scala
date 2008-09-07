package metascala.test

object NatsTest {
  import Nats._
  import Utils._
  
  type T1 = Equal[_0, _0 + _0]
  type T2 = Equal[_1, _1 + _0]
  type T3 = Equal[_1, _0 + _1]
  type T4 = Equal[_2, _1 + _1]
  type T5 = Equal[_7, _3 + _4]
}
