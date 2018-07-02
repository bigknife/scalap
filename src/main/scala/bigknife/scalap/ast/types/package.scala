package bigknife.scalap.ast

package object types {
  type ValueSet = LinkedHashSet[Value]
  object ValueSet {
    def apply(values: Value*): ValueSet = {
      val empty = LinkedHashSet.empty[Value]("Values")
      values.foldLeft(empty) { (acc, n) =>
        acc + n
      }
    }

    def empty: ValueSet = apply()

    def toBytes(valueSet: ValueSet): Array[Byte] = {
      val v: Vector[Array[Byte]] = valueSet.toVector.map(_.bytes)
      v.foldLeft(Array.emptyByteArray) { _ ++ _ }
    }
  }

  type NominateNewValuesResult  = BoolResult[NominateTracker]
  type NominationEnvelope       = Envelope[Message.Nomination]
  type NominationEnvelopeResult = BoolResult[NominationEnvelope]

  type BallotEnvelope       = Envelope[Message.BallotMessage]

  type NominationStatement = Statement[Message.Nomination]

  type Predicate[A]                     = A => Boolean
  type StatementPredicate[M <: Message] = Predicate[Statement[M]]

  type BallotPhase = BallotTracker.Phase
  val ballotPhrasePrepare: BallotPhase     = BallotTracker.Phase.Prepare
  val ballotPhraseConfirm: BallotPhase     = BallotTracker.Phase.Confirm
  val ballotPhraseExternalize: BallotPhase = BallotTracker.Phase.Externalize

  object implicits extends Delta.Syntax with Message.Ops
}
