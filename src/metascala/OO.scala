package metascala

object OO {
  import HLists._
  import Nats._
  
  trait MethodBase {
    type Object
  }
  
  trait MethodObj[Obj] extends MethodBase {
    type Object = Obj
  }
  
  trait Method0Base[Obj] extends MethodObj[Obj] {
    type Out
    def apply(obj : Obj) : Out
  }
  
  class Method0[Obj, O](fn : Obj => O) extends Method0Base[Obj] {
    type Out = O
    def apply(obj : Obj) = fn(obj)
  }
  
  trait Method1Base[Obj] extends MethodObj[Obj] {
    type In1
    type Out
    def apply(obj : Obj, arg : In1) : Out
  }
  
  class Method1[Obj, I, O](fn : (Obj, I) => O) extends Method1Base[Obj] {
    type In1 = I
    type Out = O
    def apply(obj : Obj, arg : In1) = fn(obj, arg)
  }
  
  def _override[M <: MethodBase, H, T <: HList](obj : HCons[H, T], m : M)(implicit fn : ReplaceByTypeFn[HCons[H, T], _0, M]) : HCons[H, T] =
    obj.replaceByType(_0, m)
  
    
  case class RichHCons[H, T <: HList](l : HCons[H, T]) {
    def get[M <: MethodBase](implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M = l.getByType[_0, M]
    def call[M <: Method0Base[HCons[H, T]]](implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = get[M].apply(l)
    def call[M <: Method1Base[HCons[H, T]]](arg : M#In1)(implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = applyArg(get[M], arg)
    def delegate[M <: Method0Base[HCons[H, T]]](l2 : HCons[H, T])(implicit fn : GetByTypeFn[HCons[H, T], _0, M]) : M#Out = get[M].apply(l2)
    def applyArg[M <: Method1Base[HCons[H, T]]](m : M, arg : M#In1) : M#Out = m(l, arg.asInstanceOf[m.In1])
    def |=[M <: MethodBase](m : M)(implicit fn : ReplaceByTypeFn[HCons[H, T], _0, M]) = _override(l, m)
  }
  
  implicit def hconsToRichHCons[H, T <: HList](l : HCons[H, T]) : RichHCons[H, T] = RichHCons(l)
  
}
