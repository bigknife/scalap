package bigknife.scalap.ast

package object types {
  type ValueSet = LinkedHashSet[Value]
  object ValueSet {
    def apply(values: Value*): ValueSet = {
      val empty = LinkedHashSet.empty[Value]("Values")
      values.foldLeft(empty) {(acc, n) =>
        acc + n
      }
    }
  }

  type NominateNewValuesResult = BoolResult[NominateTracker]
}
