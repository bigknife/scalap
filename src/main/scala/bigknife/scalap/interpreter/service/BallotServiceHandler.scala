package bigknife.scalap
package interpreter
package service

import ast.service._
import bigknife.scalap.ast.types._

class BallotServiceHandler extends BallotService.Handler[Stack] {
  override def newBallot(tracker: BallotTracker, value: Value, counter: Int): Stack[Ballot] =
    Stack {
      // if we have committed something, only bump the counter
      if (tracker.highBallotNotNull) Ballot(counter, tracker.high.value)
      else Ballot(1, value)
    }

  override def computeBallotTimeout(tracker: BallotTracker): Stack[Long] = Stack {setting =>
    // linear
    val sec = tracker.current.counter
    if (sec >= setting.maxTimeoutSeconds) setting.maxTimeoutSeconds * 1000L
    else sec * 1000L
  }

  override def startBallotTimer(nodeID: NodeID, slotIndex: SlotIndex, timeout: Long): Stack[Unit] = Stack {setting =>

  }
}

object BallotServiceHandler {
  private val _instance: BallotServiceHandler = new BallotServiceHandler

  trait Implicits {
    implicit val ballotServiceHandler: BallotServiceHandler = _instance
  }

}
