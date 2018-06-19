package bigknife.scalap.ast.types

/** the state tracker of nomination protocol
  */
case class NominateTracker(
    voted: Vector[Value], // X, the set of value that node v has voted to nominate
    accepted: Vector[Value], // Y, the set of value that node v has accepted as nominated
    candidates: Vector[Value], // Z, the set of value that node v considers candidate values
    latestNominations: Map[Node.ID, NominationMessage], // the set of latest Nomination message received from each node
    roundNumber: Int,
    lastEmittedMessage: Option[NominationMessage], // last message emitted by this node
    roundLeaders: Vector[Node.ID], // nodes from quorum set that have the highest priority this round
    nominationStarted: Boolean, // true if nominate protocol start
    latestCompositedCandidate: Option[Value], // the latest candidate value
    previousValue: Option[Value] // the value from the previous slot
)

object NominateTracker {
  val Empty: NominateTracker = NominateTracker(
    Vector.empty,
    Vector.empty,
    Vector.empty,
    Map.empty,
    0,
    None,
    Vector.empty,
    nominationStarted = false,
    None,
    None
  )
}
