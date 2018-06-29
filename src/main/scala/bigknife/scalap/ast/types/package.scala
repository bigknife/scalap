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

    def toBytes(valueSet: ValueSet): Array[Byte] = {
      val v: Vector[Array[Byte]] = valueSet.toVector.map(_.bytes)
      v.foldLeft(Array.emptyByteArray) {_ ++ _}
    }
  }

  type NominateNewValuesResult = BoolResult[NominateTracker]
  type NominationEnvelope = Envelope[Message.Nomination]
  type NominationEnvelopeResult = BoolResult[NominationEnvelope]

}
