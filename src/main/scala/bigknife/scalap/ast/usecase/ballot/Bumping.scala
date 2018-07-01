package bigknife.scalap.ast.usecase.ballot
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.ModelSupport
import bigknife.sop._
import bigknife.sop.implicits._

trait Bumping[F[_]] extends BallotCore[F] {
  self: ModelSupport[F] with BumpingHelper[F] =>

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
      _ <- if ((!force && tracker.currentBallotNotNull) || tracker.isExternalizePhrase) ().pureSP[F] else
    } yield ()
  }
}
