package bigknife.scalap.ast.types

sealed trait Statement[M <: Message] {
  def nodeID: NodeID // v, the node which the statement issued by
  def slotIndex: SlotIndex // i, slot index
  def quorumSetHash: Hash // D, hash of quorum set
  def message: M
  def toBytes: Array[Byte]

  // operations
  def newerThan(that: Statement[M]): Boolean = ???
}

object Statement {
  case class Nominate(
      nodeID: NodeID,
      slotIndex: SlotIndex,
      quorumSetHash: Hash,
      message: Message.Nomination
  ) extends Statement[Message.Nomination] {
    lazy val toBytes: Array[Byte] = {

      nodeID.bytes ++ slotIndex.bytes ++ quorumSetHash.bytes ++ ValueSet
        .toBytes(message.voted) ++ ValueSet.toBytes(message.accepted)
    }
  }

  def fakeNominate: Nominate =
    Nominate(NodeID(Array.emptyByteArray),
             SlotIndex(-1),
             Hash(Array.emptyByteArray),
             Message.nominationBuilder().build())

}
