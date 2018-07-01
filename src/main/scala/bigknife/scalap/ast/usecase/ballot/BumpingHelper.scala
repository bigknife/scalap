package bigknife.scalap.ast.usecase
package ballot

import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.scalap.ast.types._

trait BumpingHelper[F[_]] {
  self: ModelSupport[F] =>

  def bumpState(nodeID: NodeID,
                slotIndex: SlotIndex,
                value: Value): SP[F, Unit]

}
