package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcessHelper[F[_]] {

  /**
    * statement is newer than that saved in tracker
    * @param statement statement to be determined newer or not
    * @param tracker tracker
    * @return
    */
  def isNewerStatement(statement: Statement[Message.BallotMessage],
                       tracker: BallotTracker): Boolean = {
    !tracker.latestBallotEnvelope.contains(statement.nodeID) ||
    statement.newerThan(
      tracker.latestBallotEnvelope(statement.nodeID).statement)
  }

  /**
    * validate values of ballot statement
    * @param statement ballot message statement
    * @return
    */
  def validateValues(statement: Statement[Message.BallotMessage]): SP[F, Value.Validity] = {
    ???
  }

  /**
    * advance the slot
    * @param statement
    * @param tracker
    * @return
    */
  def advanceSlot(statement: Statement[Message.BallotMessage], tracker: BallotTracker): SP[F, BallotTracker] = ???

  def recordEnvelope(tracker: BallotTracker, envelope: Envelope[Message.BallotMessage]): SP[F, BallotTracker] = ???

  def getWorkingBallot(statement: Statement[Message.BallotMessage]): SP[F, Ballot] = ???
}
