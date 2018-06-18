package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.Message.NominationStatement
import bigknife.scalap.ast.types.{Node, NominationMessage, Slot}
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

  /**
    * get latest nomination statement of a slot, if not found, use default.
    * @param slot slot
    * @param default default nomination statement
    * @return
    */
  def latestNominationStatement(slot: Slot, default: NominationStatement): P[F, NominationStatement]

  /** let slot track the new coming nomination message
    *
    * @param slot slot
    * @param nominationMessage coming nomination message
    * @return
    */
  def trackNewNominationMessage(slot: Slot, nominationMessage: NominationMessage): P[F, Slot]
}
