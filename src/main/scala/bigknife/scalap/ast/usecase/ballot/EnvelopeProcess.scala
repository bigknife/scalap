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
  override def processBallotEnvelope[M <: BallotMessage](
      nodeID: NodeID,
      envelope: BallotEnvelope[M]): SP[F, Envelope.State] = {
    val remoteNodeID = envelope.statement.nodeID
    val slotIndex    = envelope.statement.slotIndex
    val statement    = envelope.statement
    val message      = statement.message

    for {
      _        <- logService.info(s"start to process ballot envelope $nodeID[${slotIndex}]...")
      tracker  <- nodeStore.getBallotTracker(nodeID, slotIndex)
      _        <- logService.info(s"current tracker: ${tracker.logString}")
      verified <- envelopeService.verifyEnvelopeSignature(envelope)
      _        <- logService.debug(s"ballot envelope signature verified ? $verified", Some("blt-msg-proc"))
      valid    <- self.validateValues(statement)
      _        <- logService.debug(s"ballot statement is valid ? $valid", Some("blt-msg-proc"))
      sane     <- message.isSane(nodeID == remoteNodeID).pureSP[F]
      _        <- logService.debug(s"ballot statement is sane ? $sane", Some("blt-msg-proc"))
      newer    <- self.isNewerStatement(statement, tracker).pureSP[F]
      _        <- logService.debug(s"ballot statement is newer ? $newer", Some("blt-msg-proc"))
      state <- ifM[Envelope.State](Envelope.State.invalid, _ => verified && sane && valid && newer) {
        _ =>
          for {
            qSet <- nodeStore.getQuorumSet(nodeID)
            trackerWithState <- if (tracker.phase.notExternalize) for {
              trackerD11 <- recordEnvelope(tracker, envelope)
              _ <- logService.debug(s"start to advance $nodeID $slotIndex ...",
                                    Some("blt-msg-proc"))
              trackerD12 <- self.advanceSlot(trackerD11, qSet, statement)
              _          <- logService.debug(s"advanced $nodeID $slotIndex", Some("blt-msg-proc"))
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
            _ <- nodeStore.saveBallotTracker(nodeID, trackerWithState._1)
            _ <- logService.info(
              s"end processing ballot envelope $nodeID[${slotIndex}] with state ${trackerWithState._2}," +
                s"tracker: ${trackerWithState._1.logString}")
          } yield trackerWithState._2
      }
    } yield state
  }
}
