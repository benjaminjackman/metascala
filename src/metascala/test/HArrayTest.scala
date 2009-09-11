package metascala.test

object HArrayTest {
  import metascala.TLists._
  import metascala.Nats._
  import metascala.HArrays._

  val t : HArray[Int :: Boolean :: String :: TNil] = arrayTuple(10, true, "Hello")
  val t2 : HArray[Double :: Int :: Boolean :: String :: TNil] = 10.1 :: t
  val t3 : HArray[Double :: Int :: Boolean :: String :: Int :: Boolean :: String :: TNil] = t2 ::: t
  val t4 : HArray[String :: Int :: Boolean :: String :: TNil] = "x" :: t
  val t5 : HArray[Int :: TNil] = arrayTuple(10)
  val t6 : HArray[String :: TNil] = arrayTuple("a")
  val t7 : HArray[Int :: String :: TNil] = t5 ::: t6
  val t8 : Boolean = t(_1)
  val t9 : HArray[String :: Boolean :: Int :: TNil] = t.reverse
  val t10 : HArray[Double :: Int :: Boolean :: String :: Boolean :: String :: TNil] = t3.removeNth(_4)
  val t11 : HArray[Double :: Int :: Boolean :: Boolean :: String :: Boolean :: String :: TNil] = t10.insert(_3, false)
}
