package bigknife.scalap.ast.types

/**
  * Nomination process state tracker
  */
case class NominateTracker(
    nodeID: NodeID,
    slotIndex: SlotIndex,
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
