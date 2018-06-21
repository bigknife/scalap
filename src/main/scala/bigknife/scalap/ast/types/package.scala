package bigknife.scalap.ast

package object types {

  type StatementMessage  = Message[Message.Statement]
  type NominationMessage = Message[Message.NominationStatement]
  type BallotMessage     = Message[Message.BallotStatement]
  type MessageState      = Message.State

  type NominationStatement        = Message.NominationStatement
  type BallotStatement            = Message.BallotStatement
  type BallotPrepareStatement     = Message.Prepare
  type BallotConfirmStatement     = Message.Confirm
  type BallotExternalizeStatement = Message.Externalize

  type StatementPredict = Message.Statement.Predict

  object MessageState {
    def valid: MessageState   = Message.State.valid
    def invalid: MessageState = Message.State.invalid
  }

}
