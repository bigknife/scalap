package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.Message.NominationStatement
import bigknife.scalap.ast.types.{Node, NominationMessage, Slot, Value}
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

  /**
    * slot nomination accept a value when the value is fully validated
    * @param slot slot
    * @param value value full validated
    * @return
    */
  def acceptNomination(slot: Slot, value: Value): P[F, Slot]

  /**
    * slot nomination vote a value
    * @param slot slot
    * @param value value full validated
    * @return
    */
  def voteNomination(slot: Slot, value: Value): P[F, Slot]

  /**
    * make a candidate value for nomination
    * @param slot slot
    * @param value candidate value
    * @return
    */
  def candidateValue(slot: Slot, value: Value): P[F, Slot]

  /**
    * compute a value in a slot
    * @param slot slot
    * @param value value
    * @return
    */
  def computeValueHash(slot: Slot, value: Value): P[F, Long]

  /**
    * emit nomination message for slot
    * @param slot slot
    * @param message nomination message
    * @return
    */
  def emitNominateMessage(slot: Slot, message: NominationMessage): P[F, Slot]

  /**
    * compute two slot,determine they are same or not, only concern voted and accepted, candidates ignored
    * @param s1 slot 1
    * @param s2 slot 2
    * @return
    */
  def hasBeenModifiedInNomination(s1: Slot, s2: Slot): P[F, Boolean]

  /**
    * from the second parameter's point of view if has new candidate  relative to the first parameter
    * @param s1 first slot
    * @param s2 second slot
    * @return
    */
  def hasNewCandidates(s1: Slot, s2: Slot): P[F, Boolean]

  /**
    * update the slot's nomination tracker's latest composite value
    * @param slot slot
    * @param compositeValue combined value
    * @return
    */
  def updateCompositeCandidateValue(slot: Slot, compositeValue: Value): P[F, Slot]
}
