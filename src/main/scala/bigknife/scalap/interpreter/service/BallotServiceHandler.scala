package bigknife.scalap
package interpreter
package service

import ast.service._
import bigknife.scalap.ast.types.{Ballot, BallotTracker, Value}

class BallotServiceHandler extends BallotService.Handler[Stack] {
  override def newBallot(tracker: BallotTracker, value: Value): Stack[Ballot] =
    Stack {
      // if we have committed something, only bump the counter
      if (tracker.highBallotNotNull) Ballot(tracker.high.counter + 1, tracker.high.value)
      else Ballot(1, value)
    }
}

object BallotServiceHandler {
  private val _instance: BallotServiceHandler = new BallotServiceHandler

  trait Implicits {
    implicit val ballotServiceHandler: BallotServiceHandler = _instance
  }

}
