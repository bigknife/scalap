package bigknife.scalap.ast.types

/** the state tracker of nomination protocol
  */
case class NominateTracker(
    voted: Set[Value], // X, the set of value that node v has voted to nominate
    accepted: Set[Value], // Y, the set of value that node v has accepted as nominated
    candidates: Set[Value], // Z, the set of value that node v considers candidate values
    latestNominations: Map[Node.ID, NominationMessage], // the set of latest Nomination message received from each node
    roundNumber: Int,
    lastEmittedMessage: Option[NominationMessage], // last message emitted by this node
    roundLeaders: Set[Node.ID], // nodes from quorum set that have the highest priority this round
    nominationStarted: Boolean, // true if nominate protocol start
    latestCompositedCandidate: Option[Value], // the latest candidate value
    previousValue: Option[Value] // the value from the previous slot
)
