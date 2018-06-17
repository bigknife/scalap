package bigknife.scalap.ast.types

/** the state of a slot
  *
  */
case class Slot(
    nodeId: Node.ID,
    index: Long,
    nominateTracker: NominateTracker,
    ballotTracker: BallotTracker
)
