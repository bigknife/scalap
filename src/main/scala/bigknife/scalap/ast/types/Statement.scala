package bigknife.scalap.ast.types

sealed trait Statement[M <: Message] {
  def nodeID: NodeID       // v, the node which the statement issued by
  def slotIndex: SlotIndex // i, slot index
  def quorumSetHash: Hash  // D, hash of quorum set
  def message: M
  def toBytes: Array[Byte]
}

object Statement {
  case class Nominate(
      nodeID: NodeID,
      slotIndex: SlotIndex,
      quorumSetHash: Hash,
      message: Message.Nomination
  ) extends Statement[Message.Nomination] {
    lazy val toBytes: Array[Byte] = ???
  }

}
