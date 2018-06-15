package bigknife.scalap.ast.store

import bigknife.scalap.ast.types.{Node, Slot}
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait SlotStore[F[_]] {
  /**
    * get a slot with index for a node.
    * @param nodeId node id
    * @param slotIndex slot index
    * @return a slot or none.
    */
  def getSlotOfNode(nodeId: Node.ID, slotIndex: Long): P[F, Option[Slot]]

  /**
    * save slot for a node
    * @param nodeId node id
    * @param slotIndex slot index
    * @param slot slot
    * @return
    */
  def saveSlotForNode(nodeId: Node.ID, slotIndex: Long, slot: Slot): P[F, Unit]
}
