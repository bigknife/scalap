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
  sealed trait BallotStatement[+M <: Message.BallotMessage] extends Statement[M] {
    def order: Int = message.order
  }

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

  def newerThan[M <: Message](old: Statement[M], n: Statement[M]): Boolean = {
    old match {
      case xOld: Statement.Nominate =>
        n match {
          case xN: Statement.Nominate =>
            val voted = xN.message.voted
            val oldVoted = xOld.message.voted
            val accepted = xN.message.accepted
            val oldAccepted = xOld.message.accepted
            if (voted.hasGrownEqualFrom(oldVoted) &&
              accepted.hasGrownEqualFrom(oldAccepted)) {
              voted.hasGrownFrom(oldVoted) || accepted.hasGrownFrom(oldAccepted)
            } else false
          case _ => false
        }
      case xOld: Statement.BallotStatement[_] =>
        n match {
          case xN: Statement.BallotStatement[_] if xN.order != xOld.order => xN.order > xOld.order
          case xN: Statement.Prepare => xOld match {
            case xOld1: Statement.Prepare =>
              if(xOld1.message.ballot < xN.message.ballot) true
              else if(xOld1.message.ballot == xN.message.ballot) {
                if(xOld1.message.prepared < xN.message.prepared) true
                else if (xOld1.message.prepared == xN.message.prepared) {
                  if (xOld1.message.preparedPrime < xN.message.preparedPrime) true
                  else if (xOld1.message.preparedPrime == xN.message.preparedPrime) {
                    xOld1.message.hCounter < xN.message.hCounter
                  } else false
                } else false
              } else false
            case _ => false
          }

          case xN: Statement.Commit => xOld match {
            case xOld1: Statement.Commit =>
              if (xOld1.message.ballot < xN.message.ballot) true
              else if (xOld1.message.ballot == xN.message.ballot) {
                if(xOld1.message.preparedCounter == xN.message.preparedCounter) xOld1.message.hCounter < xN.message.hCounter
                else xOld1.message.preparedCounter < xN.message.preparedCounter
              }
              else false
            case _ => false
          }
          case _: Statement.Externalize => false

          case _ => false
        }
      case _ => false
    }
  }

  def initialPrepare(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Prepare =
    Prepare(nodeID, slotIndex, quorumSet.hash, Message.nullPrepare)

  def initialCommit(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Commit =
    Commit(nodeID, slotIndex, quorumSet.hash, Message.nullCommit)

  def initialExternalize(nodeID: NodeID, slotIndex: SlotIndex, quorumSet: QuorumSet): Externalize =
    Externalize(nodeID, slotIndex, quorumSet.hash, Message.nullExternalize)

}
