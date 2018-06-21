package bigknife.scalap.interpreter

import bigknife.scalap.ast.types._

case class Setting(
    nodeId: Node.ID,
    quorumSet: QuorumSet,
    maxNodesInQuorumSet: Int = 1000,
    maxBallotMessageLevel: Int = 50
)
