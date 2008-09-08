package metascala

object TypeSets {
  trait TypeSet {
    type Contains
  }

  trait EmptyTypeSet extends TypeSet {

  }
}
