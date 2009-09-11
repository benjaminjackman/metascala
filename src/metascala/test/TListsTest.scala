package metascala.test

object TListsTest {
  import metascala.Utils._
  import metascala.TLists._
  import metascala.Nats._

  type T1 = Int :: Boolean :: TNil
  type T2 = Float :: (Int, Double) :: TNil
  type T3 = T1 ::: T2
  type T4 = Equal[T3, Int :: Boolean :: Float :: (Int, Double) :: TNil]
  type T5 = Equal[T3#RemoveNth[_2], Int :: Boolean :: (Int, Double) :: TNil]
}
