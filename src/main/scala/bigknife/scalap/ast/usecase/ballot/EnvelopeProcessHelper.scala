package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast
import bigknife.scalap.ast.types
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcessHelper[F[_]] {
  self: ModelSupport[F] with ConvenienceSupport[F] =>
  import model._

  /**
    * statement is newer than that saved in tracker
    * @param statement statement to be determined newer or not
    * @param tracker tracker
    * @return
    */
  def isNewerStatement[M <: BallotMessage](statement: BallotStatement[M],
                                           tracker: BallotTracker): Boolean = {
    val old: BallotEnvelope[BallotMessage] = tracker.latestBallotEnvelope(statement.nodeID)

    !tracker.latestBallotEnvelope.contains(statement.nodeID) || Statement.newerThan(old.statement,
                                                                                    statement)

  }

  /**
    * validate values of ballot statement
    * @param statement ballot message statement
    * @return
    */
  def validateValues[M <: BallotMessage](
      statement: Statement.BallotStatement[M]): SP[F, Boolean] = {
    statement match {
      case x: Statement.Prepare =>
        val msg: Message.Prepare = x.message
        for {
          b <- envelopeService.validateValue(msg.ballot.value)
          p <- ifM[Boolean](true, _ => msg.prepare.value.notEmpty) { _ =>
            envelopeService.validateValue(msg.prepare.value)
          }
        } yield b && p
      case x: Statement.Commit =>
        envelopeService.validateValue(x.message.ballot.value)
      case x: Statement.Externalize =>
        envelopeService.validateValue(x.message.commit.value)
    }
  }

  /**
    * advance the slot
    * @param statement
    * @param tracker
    * @return
    */
  def advanceSlot[M <: BallotMessage](statement: BallotStatement[M],
                                      tracker: BallotTracker): SP[F, BallotTracker] = {
    ???
  }

  def recordEnvelope[M <: BallotMessage](tracker: BallotTracker,
                                         envelope: BallotEnvelope[M]): SP[F, BallotTracker] =
    for {
      t <- ballotService.recordEnvelope(tracker, envelope)
      _ <- nodeStore.saveHistoricalStatement(envelope.statement)
    } yield t

  def getWorkingBallot[M <: BallotMessage](statement: BallotStatement[M]): SP[F, Ballot] = ???
}
