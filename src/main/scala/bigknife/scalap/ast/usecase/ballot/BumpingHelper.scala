package bigknife.scalap.ast.usecase
package ballot

import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._

trait BumpingHelper[F[_]] {
  self: ModelSupport[F] with ConvenienceSupport[F]=>

  protected def updateCurrentBallotForTracker(tracker: BallotTracker, newB: Ballot): SP[F, Delta[BallotTracker]] = {
    // only update when in prepare phase or confirm phase
    ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _.data.notExternalizePhrase) {x =>
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


  protected def checkHeardFromQuorum(tracker: BallotTracker): SP[F, Delta[BallotTracker]] = ???

}
