package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait BallotBaseHelper[F[_]] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BallotCore[F] =>
  import model._

  protected def emitCurrentStateStatement(tracker: BallotTracker,
                                          quorumSet: QuorumSet): SP[F, Delta[BallotTracker]] = {
    // create a message from tracker
    // check the generated msg has been processed
    // if hasn't been processed, process it locally
    // then check the msg can be emitted or not

    def hasProcessed(msg: BallotEnvelope[BallotMessage]): Boolean =
      tracker.latestBallotEnvelope.exists {
        case (nodeId, env) => nodeId == tracker.nodeID && env == msg
      }

    for {
      envelope <- ballotService.createBallotEnvelope(tracker, quorumSet)
      trackerD0 <- ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _ => !hasProcessed(envelope)) {
        x =>
          for {
            _                <- nodeStore.saveBallotTracker(tracker.nodeID, tracker)
            state            <- self.processBallotEnvelope(tracker.nodeID, envelope)
            trackerD1 <- ifM[Delta[BallotTracker]](x, _ => state == Envelope.State.Valid) {_ =>
              for {
                trackerProcessed <- nodeStore.getBallotTracker(tracker.nodeID, tracker.slotIndex)
                trackerD10        <- ballotService.broadcastEnvelope(trackerProcessed, quorumSet, envelope)
              } yield trackerD10
            }
          } yield trackerD1
      }
    } yield trackerD0
  }
}
