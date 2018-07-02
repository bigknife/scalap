package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._
import bigknife.scalap.ast.usecase._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * processing ballot envelope
  */
trait EnvelopeProcess[F[_]] extends BallotCore[F] {
  self: ModelSupport[F]
    with ConvenienceSupport[F]
    with BallotBaseHelper[F]
    with EnvelopeProcessHelper[F] =>
  import model._

  /**
    * process ballot message envelope
    *
    * @param nodeID   node that processing the envelope
    * @param envelope coming envelope that node received
    * @return envelope.state
    */
  override def processBallotEnvelope(nodeID: NodeID,
                                     envelope: BallotEnvelope): SP[F, Envelope.State] = {
    val nodeID    = envelope.statement.nodeID
    val slotIndex = envelope.statement.slotIndex
    val statement = envelope.statement
    val message   = statement.message

    for {
      tracker  <- nodeStore.getBallotTracker(nodeID, slotIndex)
      verified <- envelopeService.verifyEnvelopeSignature(envelope)
      valid    <- self.validateValues(statement)
      state <- ifM[Envelope.State](
        Envelope.State.invalid,
        _ =>
          verified && message.isSane && valid == Value.Validity.FullyValidated && self
            .isNewerStatement(statement, tracker)) { _ =>
        for {
          qSet <- nodeStore.getQuorumSet(nodeID)
          trackerWithState <- if (tracker.phase.notExternalize) for {
            trackerD11 <- recordEnvelope(tracker, envelope)
            trackerD12 <- self.advanceSlot(statement, trackerD11)
          } yield (trackerD12, Envelope.State.valid)
          else
            for {
              workingBallot <- self.getWorkingBallot(statement)
              trackerD11WithState <- ifM[(BallotTracker, Envelope.State)](
                (tracker, Envelope.State.invalid),
                _._1.commit.compatible(workingBallot)) { x =>
                for {
                  x0 <- self.recordEnvelope(x._1, envelope)
                } yield (x0, Envelope.State.valid)
              }
            } yield trackerD11WithState
        } yield trackerWithState._2
      }
    } yield state
  }
}
