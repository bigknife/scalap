package bigknife.scalap
package interpreter
package service

import ast.service._
import bigknife.scalap.ast.types.{Ballot, BallotTracker, Value}

class BallotServiceHandler extends BallotService.Handler[Stack] {
  override def newBallot(tracker: BallotTracker, value: Value, counter: Int): Stack[Ballot] =
    Stack {
      if (tracker.highBallotNotNull) Ballot(counter, tracker.high.value)
      else Ballot(counter, value)
    }
}

object BallotServiceHandler {
  private val _instance: BallotServiceHandler = new BallotServiceHandler

  trait Implicits {
    implicit val ballotServiceHandler: BallotServiceHandler = _instance
  }

}
