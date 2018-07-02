package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.sop._

trait NominationCore[F[_]] {
  /**
    * start a nomination round for a node to vote value at a specified index
    * @param nodeID node id
    * @param slotIndex slot index
    * @param valueToNominate value to nominate
    * @param previousValue the value of the slot[slotIndex -1]
    * @return
    */
  def nominate(nodeID: NodeID,
               slotIndex: SlotIndex,
               valueToNominate: Value,
               previousValue: Value): SP[F, Boolean]

  /**
    * handle the message when received from self or peers
    * @param envelope message envelope
    * @return
    */
  def processNominationEnvelope(nodeID: NodeID, envelope: NominationEnvelope): SP[F, Envelope.State]
}
