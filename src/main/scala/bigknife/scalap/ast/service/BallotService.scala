package bigknife.scalap.ast
package service

import types._

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait BallotService[F[_]] {
  /**
    * create an appropriate ballot.
    * if tracker's high ballot is not empty, the coming value is ignored
    * @param tracker tracker
    * @param value coming value
    * @return
    */
  def newBallot(tracker: BallotTracker, value: Value): P[F, Ballot]
}
