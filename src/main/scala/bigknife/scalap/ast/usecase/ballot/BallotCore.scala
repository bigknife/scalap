package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types._
import bigknife.sop._

trait BallotCore[F[_]] {

  /**
    * bump a candidate value(combined)
    * @param value combined candidates
    * @param force force update ballot's tracker
    * @return
    */
  def bumpState(nodeID: NodeID, slotIndex: SlotIndex, value: Value, force: Boolean): SP[F, Unit]
}
