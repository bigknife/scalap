package bigknife.scalap.ast.types

import implicits._

sealed trait Statement[+M <: Message] {
  def nodeID: NodeID       // v, the node which the statement issued by
  def slotIndex: SlotIndex // i, slot index
  def quorumSetHash: Hash  // D, hash of quorum set
  def message: M
}

object Statement {
  sealed trait NominationStatement                          extends Statement[Message.Nomination]
  sealed trait BallotStatement[+M <: Message.BallotMessage] extends Statement[M]

  case class Nominate(
      nodeID: NodeID,
      slotIndex: SlotIndex,
      quorumSetHash: Hash,
      message: Message.Nomination
  ) extends NominationStatement

  case class Prepare(nodeID: NodeID,
                     slotIndex: SlotIndex,
                     quorumSetHash: Hash,
                     message: Message.Prepare)
      extends BallotStatement[Message.Prepare]

  case class Commit(nodeID: NodeID,
                    slotIndex: SlotIndex,
                    quorumSetHash: Hash,
                    message: Message.Commit)
      extends BallotStatement[Message.Commit]

  case class Externalize(nodeID: NodeID,
                         slotIndex: SlotIndex,
                         quorumSetHash: Hash,
                         message: Message.Externalize)
      extends BallotStatement[Message.Externalize]

  def fakeNominate: Nominate =
    Nominate(NodeID(Array.emptyByteArray),
             SlotIndex(-1),
             Hash(Array.emptyByteArray),
             Message.nominationBuilder().build())

  def newerThan[M <: Message](old: Statement[M], n: Statement[M]): Boolean = ???

  def initialPrepare(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Prepare =
    Prepare(nodeID, slotIndex, quorumSet.hash, Message.nullPrepare)

  def initialCommit(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Commit =
    Commit(nodeID, slotIndex, quorumSet.hash, Message.nullCommit)

  def initialExternalize(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Externalize =
    Externalize(nodeID, slotIndex, quorumSet.hash, Message.nullExternalize)

}
