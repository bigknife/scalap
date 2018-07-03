package bigknife.scalap
package interpreter
package service

import ast.service._
import bigknife.scalap.ast.types._
import java.util.{Timer, TimerTask}

import org.slf4j.{Logger, LoggerFactory}

private[service] class BallotServiceHandler extends BallotService.Handler[Stack] {
  val lock = new Object

  val timerCache: collection.mutable.Map[(NodeID, SlotIndex), Timer] = collection.mutable.Map.empty

  val log: Logger = LoggerFactory.getLogger(getClass)

  override def newBallot(tracker: BallotTracker, value: Value, counter: Int): Stack[Ballot] =
    Stack {
      // if we have committed something, only bump the counter
      if (tracker.highBallotNotNull) Ballot(counter, tracker.high.value)
      else Ballot(1, value)
    }

  override def recordEnvelope[M <: BallotMessage](
      tracker: BallotTracker,
      envelope: BallotEnvelope[M]): Stack[BallotTracker] =
    Stack {
      tracker.copy(
        latestBallotEnvelope = tracker.latestBallotEnvelope + (envelope.statement.nodeID -> envelope))
    }

  override def computeBallotTimeout(tracker: BallotTracker): Stack[Long] = Stack { setting =>
    // linear
    val sec = tracker.current.counter
    if (sec >= setting.maxTimeoutSeconds) setting.maxTimeoutSeconds * 1000L
    else sec * 1000L
  }

  override def startBallotTimer(nodeID: NodeID, slotIndex: SlotIndex, timeout: Long): Stack[Unit] =
    Stack { setting =>
      lock.synchronized {
        val timer = if (timerCache.contains((nodeID, slotIndex))) {
          timerCache((nodeID, slotIndex))
        } else {
          val t = new Timer("ballot-timer")
          timerCache.put((nodeID, slotIndex), t)
          t
        }
        timer.schedule(
          new TimerTask {
            override def run(): Unit = {
              log.debug(
                s"start ballot timer for $nodeID-Slot[$slotIndex] with timeout = ${timeout}ms")
              util.ec.submitBallotTask {
                setting.connect.runAbandonBallot(nodeID, slotIndex, counter = 0)
              }
            }
          },
          timeout
        )
      }

    }

  override def stopBallotTimer(nodeID: NodeID, slotIndex: SlotIndex): Stack[Unit] = Stack {
    setting =>
      lock.synchronized {
        val timer = timerCache.get((nodeID, slotIndex))
        timer.foreach(_.cancel())
        timerCache.remove((nodeID, slotIndex))
        ()
      }
  }

  override def setPrepared(tracker: BallotTracker, ballot: Ballot): Stack[Delta[BallotTracker]] =
    Stack {
      if (tracker.prepared.notNull) {
        if (tracker.prepared < ballot) {
          val dPreparedPrime =
            if (tracker.prepared.incompatible(ballot)) tracker.prepared else tracker.preparedPrime
          Delta.changed(tracker.copy(prepared = ballot, preparedPrime = dPreparedPrime))
        } else {
          if (tracker.prepared > ballot && (tracker.preparedPrime.notNull && tracker.preparedPrime < ballot)) {
            Delta.changed(tracker.copy(preparedPrime = ballot))
          } else Delta.unchanged(tracker)
        }
      } else Delta.changed(tracker.copy(prepared = ballot))
    }

  override def clearCommitIfNeeded(tracker: BallotTracker): Stack[Delta[BallotTracker]] = Stack {
    if (tracker.commitBallotNotNull && tracker.highBallotNotNull &&
      ((tracker.preparedBallotNotNull && tracker.high.lessAndIncompatible(tracker.prepared)) ||
        (tracker.preparedPrimeBallotNotNull && tracker.high.lessAndIncompatible(tracker.preparedPrime)))) {
      Delta.changed(tracker.copy(commit = Ballot.Null))
    } else Delta.unchanged(tracker)
  }
}

object BallotServiceHandler {
  private val _instance: BallotServiceHandler = new BallotServiceHandler

  trait Implicits {
    implicit val ballotServiceHandler: BallotServiceHandler = _instance
  }

}
