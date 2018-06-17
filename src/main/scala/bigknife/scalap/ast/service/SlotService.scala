package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.{Node, Slot}
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait SlotService[F[_]] {
  /**
    * create a slot with index for a node
    * @param nodeId node id
    * @param slotIndex slot index
    * @return a new slot
    */
  def createSlot(nodeId: Node.ID, slotIndex: Long): P[F, Slot]
}
