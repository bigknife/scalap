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
  def getSlotOfNode[A: Ordered](nodeId: Node.ID, slotIndex: Long): P[F, Option[Slot[A]]]

  /**
    * save slot for a node
    * @param nodeId node id
    * @param slot slot
    * @return
    */
  def saveSlotForNode[A: Ordered](nodeId: Node.ID, slot: Slot[A]): P[F, Unit]
}
