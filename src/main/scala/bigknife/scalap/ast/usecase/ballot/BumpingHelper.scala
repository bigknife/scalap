package bigknife.scalap.ast.usecase
package ballot

import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._

trait BumpingHelper[F[_]] {
  self: ModelSupport[F] with ConvenienceSupport[F] =>
  import model._

  protected def updateCurrentBallotForTracker(tracker: BallotTracker,
                                              newB: Ballot): SP[F, Delta[BallotTracker]] = {
    // only update when in prepare phase or confirm phase
    ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _.data.notExternalizePhrase) { x =>
      val t = x.data
      // if current is empty, then set ballot directly
      if (t.currentBallotIsNull) {
        t.copy(current = newB, heardFromQuorum = false).changed.pureSP[F]
      } else {
        assert(newB >= t.current)
        // if something committed, newB should compatible with it, or don't update anything
        // or if newB == t.current, not necessary to update
        if ((newB == t.current) || t.commitBallotNotNull && t.commit.incompatible(newB)) x.pureSP[F]
        else {
          // so now, newB > t.current
          t.copy(current = newB, heardFromQuorum = false).changed.pureSP[F]
        }
      }
    }
  }

  protected def checkHeardFromQuorum(tracker: BallotTracker): SP[F, Delta[BallotTracker]] = {
    // if tracker's current is null, nothing to do
    ifM[Delta[BallotTracker]](tracker.unchanged, _.data.currentBallotNotNull) { x =>
      // first, collect nodes that has accepted prepare.
      val nodes: Set[NodeID] = tracker.latestBallotEnvelope
        .filter {
          case (_, Envelope(statement, _)) =>
            statement match {
              case x: Message.Prepare =>
                x.prepare.counter >= tracker.current.counter
              case _ => true
            }
        }
        .keys
        .toSet

      // then, we should filter those nodes' accept prepare not ratified by it's quorum set
      // then, we got the final intact nodes, check them if they are quorum to current node's quorumset
      // if true, start or stop a ballot time according to current phase.

      def tryStartBallotTimer(isQ: Boolean, tracker: BallotTracker): SP[F, Delta[BallotTracker]] = {
        if (isQ) {
          val oldHeardFromQuorum = tracker.heardFromQuorum
          val t1                 = tracker.copy(heardFromQuorum = true)

          val startBallotTimer =
            ifM[Unit]((), _ => !oldHeardFromQuorum && tracker.notExternalizePhrase) { _ =>
              for {
                timeout <- ballotService.computeBallotTimeout(tracker)
                _       <- ballotService.startBallotTimer(tracker.nodeID, tracker.slotIndex, timeout)
              } yield ()
            }
          val delta = (if (!oldHeardFromQuorum) t1.changed else t1.unchanged).pureSP[F]
          for {
            _ <- startBallotTimer
            x <- delta
          } yield x
        } else tracker.unchanged.pureSP[F]
      }

      def tryStopBallotTimer(isQ: Boolean, tracker: BallotTracker): SP[F, Delta[BallotTracker]] = {
        val oldHeardFromQuorum = tracker.heardFromQuorum
        if (isQ) {
          val t1 = tracker.copy(heardFromQuorum = true)
          val stopBallotTimer = ifM[Unit]((), _ => tracker.notExternalizePhrase) { _ =>
            ballotService.stopBallotTimer(tracker.nodeID, tracker.slotIndex)
          }
          val delta = (if (!oldHeardFromQuorum) t1.changed else t1.unchanged).pureSP[F]
          for {
            _ <- stopBallotTimer
            x <- delta
          } yield x
        } else {
          //stopBallotTimer
          val stopBallotTimer: SP[F, Unit] =
            ballotService.stopBallotTimer(tracker.nodeID, tracker.slotIndex)
          val delta =
            if (oldHeardFromQuorum) tracker.copy(heardFromQuorum = false).changed.pureSP[F]
            else tracker.unchanged.pureSP[F]
          for {
            _ <- stopBallotTimer
            x <- delta
          } yield x
        }
      }

      // only `changed` varies
      def combine(d1: Delta[BallotTracker], d2: Delta[BallotTracker]): Delta[BallotTracker] =
        (d1.changed, d2.changed) match {
          case (false, false) => d1
          case (true, false)  => d1
          case (false, true)  => d2
          case (true, true)   => d1
        }

      for {
        ratified <- self.liftAndFilterRatifiedNodes(nodes, tracker.latestBallotEnvelope)
        qs       <- nodeStore.getQuorumSet(tracker.nodeID)
        isQ      <- qs.isQuorumSlice(ratified).pureSP[F]
        d1       <- tryStartBallotTimer(isQ, tracker)
        d2       <- tryStopBallotTimer(isQ, tracker)
      } yield combine(d1, d2)
    }
  }

}
