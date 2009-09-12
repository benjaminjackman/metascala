package metascala

object HCollections {
  import Nats._
  import TLists._

  trait Tuple[L <: TList] {
    type This[L <: TList] <: Tuple[L]

    def ::[T](v : T) : This[T :: L]
    def :::[L2 <: TList](l : This[L2]) : This[L2#Append[L]]
    def apply[N <: Nat](n : N) : L#Nth[N]
    def reverse : This[L#Reverse]
    def removeNth[N <: Nat](n : N) : This[L#RemoveNth[N]]
    def insert[N <: Nat, E](n : N, elem : E): This[L#Insert[N, E]]
//    def replaceSameType[N <: Nat, E](n : N, elem : E) : This[L]
  }

}