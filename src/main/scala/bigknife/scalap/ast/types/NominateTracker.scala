package bigknife.scalap.ast.types

/**
  * Nomination process state tracker
  */
case class NominateTracker(
    nodeID: NodeID,
    slotIndex: SlotIndex,
    nominationStarted: Boolean,
    round: Int,
    roundLeaders: Set[NodeID],
    previousValue: Value,
    nomination: Message.Nomination,
    candidates: ValueSet,
    latestNominations: Map[NodeID, Envelope[Message.Nomination]],
    lastSentEnvelope: Option[Envelope[Message.Nomination]]
) {
  def sentEnvelope(envelope: Envelope[Message.Nomination]): NominateTracker =
    copy(lastSentEnvelope = Some(envelope))
}

object NominateTracker {
  def newTracker(nodeID: NodeID, slotIndex: SlotIndex): NominateTracker =
    NominateTracker(nodeID,
                    slotIndex,
                    nominationStarted = false,
                    0,
                    Set.empty[NodeID],
                    Value.bottom,
                    Message.nominationBuilder().build(),
                    ValueSet.empty,
                    Map.empty,
                    None)
}
