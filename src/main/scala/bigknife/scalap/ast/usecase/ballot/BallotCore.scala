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

  /**
    * bump a candidate value with specified counter
    * @param nodeID node id
    * @param slotIndex slot index
    * @param value value
    * @param counter coutner, should be > 0
    * @return
    */
  def bumpState(nodeID: NodeID, slotIndex: SlotIndex, value: Value, counter: Int): SP[F, Unit]

  /**
    * abandon ballot of counter
    * @param nodeID node id
    * @param slotIndex slot index
    * @param counter counter
    * @return
    */
  def abandonBallot(nodeID: NodeID, slotIndex: SlotIndex, counter: Int): SP[F, Unit]

  /**
    * process ballot message envelope
    * @param nodeID node that processing the envelope
    * @param envelope coming envelope that node received
    * @return envelope.state
    */
  def processBallotEnvelope[M <: BallotMessage](nodeID: NodeID, envelope: BallotEnvelope[M]): SP[F, Envelope.State]
}
