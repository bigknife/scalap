package bigknife.scalap.ast

import bigknife.sop.SP

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
  }

  type NominateNewValuesResult  = BoolResult[NominateTracker]
  type NominationEnvelope       = Envelope[Message.Nomination]
  type NominationEnvelopeResult = BoolResult[NominationEnvelope]

  type BallotMessage                        = Message.BallotMessage
  type BallotEnvelope[+M <: BallotMessage]  = Envelope.BallotEnvelope[M]
  type BallotStatement[+M <: BallotMessage] = Statement.BallotStatement[M]

  type NominationStatement = Statement[Message.Nomination]

  type Predicate[A]                     = A => Boolean
  type SPPredicate[F[_], A]             = A => SP[F, Boolean]
  type StatementPredicate[M <: Message] = Predicate[Statement[M]]

  type BallotPhase = BallotTracker.Phase
  val ballotPhrasePrepare: BallotPhase     = BallotTracker.Phase.Prepare
  val ballotPhraseConfirm: BallotPhase     = BallotTracker.Phase.Commit
  val ballotPhraseExternalize: BallotPhase = BallotTracker.Phase.Externalize

  object implicits
      extends Delta.Syntax
      with Message.Ops
      with OpaqueBytes.Syntax
      with OpaqueBytesTransformers
}
