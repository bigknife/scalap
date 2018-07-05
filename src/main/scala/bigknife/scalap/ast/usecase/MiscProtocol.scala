package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

trait MiscProtocol[F[_]] {
  self: ModelSupport[F] =>
  import model._

  def getNominateTracker(nodeID: NodeID, slotIndex: SlotIndex): SP[F, NominateTracker] =
    nodeStore.getNominateTracker(nodeID, slotIndex)

  def getBallotTracker(nodeID: NodeID, slotIndex: SlotIndex): SP[F, BallotTracker] =
    nodeStore.getBallotTracker(nodeID, slotIndex)

  def cacheQuorumSet(quorumSet: QuorumSet): SP[F, Unit] =
    nodeStore.cacheQuorumSet(quorumSet)
}
