package bigknife.scalap.ast.types

import bigknife.scalap.ast.types.Message.HistoricalStatement

/** the state of a slot
  *
  */
case class Slot(
    nodeId: Node.ID,
    index: Long,
    nominateTracker: NominateTracker,
    ballotTracker: BallotTracker,
    statementHistory: Vector[HistoricalStatement],
    fullValidated: Boolean
)
