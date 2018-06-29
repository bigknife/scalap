package bigknife.scalap.ast.types

case class Envelope[M <: Message](statement: Statement[M], signature: Signature)

object Envelope {
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
  def fakeNominate: Envelope[Message.Nomination] = Envelope(Statement.fakeNominate, Signature.empty)
}
