package bigknife.scalap.ast.store

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.sop.macros._

@sp trait NodeStore[F[_]] {
  def getNominateTracker(nodeID: NodeID, slotIndex: SlotIndex): P[F, NominateTracker]
  def getQuorumSet(nodeID: NodeID): P[F, QuorumSet]
}
