package bigknife.scalap.ast.types

case class Message[+S <: Message.Statement](
    statement: S,
    signature: Signature
)
object Message {
  // message handling state
  sealed trait State

  object State {
    case object Valid extends State {
      override def toString: String = "VALID"
    }
    case object Invalid extends State {
      override def toString: String = "INVALID"
    }
    def valid: State = Valid
    def invalid: State = Invalid
  }

  sealed trait Statement {
    def nodeId: Node.ID
    def slotIndex: Long
    def quorumSetHash: Hash
  }
  sealed trait NominationStatement extends Statement
  sealed trait BallotStatement extends Statement

  // Nominate Message
  // see the paper, Nominate v, i, X, Y, D
  case class Nominate(
      nodeId: Node.ID, // v
      slotIndex: Long, // i
      votes: Vector[Value], // X
      accepted: Vector[Value], // Y
      quorumSetHash: Hash // D
  ) extends NominationStatement

  case class Externalize(
      nodeId: Node.ID, // v
      slotIndex: Long, // i
      quorumSetHash: Hash // D
  ) extends BallotStatement

  // historical statement
  case class HistoricalStatement(
      statement: Statement,
      timestamp: Long,
      validated: Boolean
  )

  object Statement {
    type Predict = Message.Statement => Boolean
    def predict(f: Message.Statement => Boolean): Predict = f
  }
}
