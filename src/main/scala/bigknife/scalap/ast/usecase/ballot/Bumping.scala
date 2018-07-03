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
    // else bump the value  to current ballot.
    // and, if in externalize phrase, can't bump
    def nextCounter(tracker: BallotTracker): Int =
      if (tracker.currentBallotIsNull) 1 else tracker.current.counter + 1
    for {
      tracker <- nodeStore.getBallotTracker(nodeID, slotIndex)
      _ <- ifM[Unit]((), _ => !force && tracker.currentBallotNotNull) { _ =>
        bumpState(nodeID, slotIndex, value, nextCounter(tracker))
      }
    } yield ()
  }

  override def bumpState(nodeID: NodeID,
                         slotIndex: SlotIndex,
                         value: Value,
                         counter: Int): SP[F, Unit] = {
    // if not forcing to bump, and current ballot is not NULL, ignore it.
    // else bump the value  to current ballot.
    // and, if in externalize phrase, can't bump
    for {
      tracker <- nodeStore.getBallotTracker(nodeID, slotIndex)
      _ <- ifM[Unit]((), _ => tracker.isExternalizePhase) { _ =>
        for {
          newB      <- ballotService.newBallot(tracker, value, counter)
          trackerD0 <- self.updateCurrentBallotForTracker(tracker, newB)
          trackerD1 <- ifM[Delta[BallotTracker]](trackerD0, _.changed) { x =>
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

  /**
    * abandon ballot of counter
    *
    * @param nodeID    node id
    * @param slotIndex slot index
    * @param counter   counter
    * @return
    */
  override def abandonBallot(nodeID: NodeID, slotIndex: SlotIndex, counter: Int): SP[F, Unit] = {
    // got current candidate value
    for {
      nominateTracker <- nodeStore.getNominateTracker(nodeID, slotIndex)
      ballotTracker   <- nodeStore.getBallotTracker(nodeID, slotIndex)
      value           <- nominateService.combineValues(nominateTracker.candidates)
      valueModified <- ifM[Value](value, _.isEmpty) { x =>
        (if (ballotTracker.currentBallotNotNull) ballotTracker.current.value else x).pureSP[F]
      }
      _ <- ifM[Unit]((), _ => valueModified.isEmpty) { _ =>
        if (counter <= 0) bumpState(nodeID, slotIndex, valueModified, force = true)
        else bumpState(nodeID, slotIndex, valueModified, counter)
      }
    } yield ()
  }
}
