package metascala.test

object UnitsTest {
  import Units._
  import Integers._
  
  val dist : Length = measure(2.3) * m
  val time : Time = measure(1.7) * s
  val speed : Speed = dist / time
}
