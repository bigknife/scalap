package bigknife.scalap.ast.types

sealed trait Envelope[+M <: Message] {
  def statement: Statement[M]
  def signature: Signature
}

object Envelope {
  case class NominationEnvelope(statement: Statement.NominationStatement, signature: Signature)
      extends Envelope[Message.Nomination]
  case class BallotEnvelope[+M <: Message.BallotMessage](statement: Statement.BallotStatement[M],
                                                        signature: Signature)
      extends Envelope[M]

  sealed trait State
  object State {
    case object Valid extends State {
      override def toString: String = "Valid"
    }
    case object Invalid extends State {
      override def toString: String = "Invalid"
    }
    def valid: State   = Valid
    def invalid: State = Invalid
  }

  ////// smart constructors
  def fakeNominate: Envelope[Message.Nomination] = NominationEnvelope(Statement.fakeNominate, Signature.empty)
}
