package bigknife.scalap.interpreter

import bigknife.scalap.ast.types._

case class Setting(
    nodeId: Node.ID,
    quorumSet: QuorumSet,
    reNominate: (Slot, ReNominateArgs) => Unit = (_, _) => (),
    maxNodesInQuorumSet: Int = 1000,
    maxBallotMessageLevel: Int = 50
)
