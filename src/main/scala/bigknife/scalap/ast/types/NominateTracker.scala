package bigknife.scalap.ast.types

/** the state tracker of nomination protocol
  */
case class NominateTracker(
    voted: Set[Value],
    accepted: Set[Value],
    candidates: Set[Value],
    latestNominations: Map[Node.ID, NominationMessage]
)
