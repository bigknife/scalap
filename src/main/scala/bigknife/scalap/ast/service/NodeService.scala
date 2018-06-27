package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.{Node, QuorumSet, Slot}
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait NodeService[F[_]] {
  def getNodePriority(slot: Slot, nodeId: Node.ID, qs: QuorumSet): P[F, Long]
}
