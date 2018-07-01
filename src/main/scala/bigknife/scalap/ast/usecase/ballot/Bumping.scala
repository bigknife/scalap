package bigknife.scalap.ast.usecase.ballot
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait Bumping[F[_]] extends BallotCore[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BumpingHelper[F] with BallotBaseHelper[F] =>

  import model._

  /**
    * bump a candidate value(combined)
    *
    * @param value combined candidates
    * @param force force update ballot's tracker
    * @return
    */
  override def bumpState(nodeID: NodeID,
                         slotIndex: SlotIndex,
                         value: Value,
                         force: Boolean): SP[F, Unit] = {
    // if not forcing to bump, and current ballot is not NULL, ignore it.
    // else bump to ballot.
    // and, if in externalize phrase, can't bump
    for {
      tracker <- nodeStore.getBallotTracker(nodeID, slotIndex)
      _ <- if ((!force && tracker.currentBallotNotNull) || tracker.isExternalizePhrase) ().pureSP[F]
      else {
        for {
          newB      <- ballotService.newBallot(tracker, value)
          trackerD0 <- self.updateCurrentBallotForTracker(tracker, newB)
          trackerD1 <- ifM[Delta[BallotTracker]](trackerD0, _.changed){x =>
            for {
              trackerD1_0 <- self.emitCurrentStatement(x.data)
              trackerD1_1 <- self.checkHeardFromQuorum(trackerD1_0.data)
            } yield trackerD1_1
          }
          _ <- nodeStore.saveBallotTracker(nodeID, trackerD1.data)
        } yield ()
      }
    } yield ()
  }
}
