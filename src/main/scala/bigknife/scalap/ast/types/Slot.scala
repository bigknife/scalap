package bigknife.scalap.ast.types

/** the state of a slot
  *
  */
case class Slot[A: Ordered[A]](
    nodeId: Node.ID,
    index: Long,
    nominateTracker: NominateTracker[A],
    ballotTracker: BallotTracker
)
