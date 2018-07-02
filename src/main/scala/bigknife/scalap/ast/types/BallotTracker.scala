package bigknife.scalap.ast.types

case class BallotTracker(
                          nodeID: NodeID,
                          slotIndex: SlotIndex,
                          phase: BallotPhase, // phi, current phrase
                          current: Ballot, // b, current ballot try to prepare and commit
                          prepare: Ballot, // p, accepted prepare
                          preparePrime: Ballot, // p', accepted preparePrime, less and incompatible to prepare
                          high: Ballot, // h, highest confirmed ballot.counter, if is 0, no confirmed
                          commit: Ballot, // c, lowest confirmed ballot.counter
                          latestBallotEnvelope: Map[NodeID, Envelope[Message.BallotMessage]], // M
                          lastSentEnvelope: Option[Envelope[Message.BallotMessage]],
                          heardFromQuorum: Boolean
) {
  def currentBallotIsNull: Boolean = current.isNull
  def currentBallotNotNull: Boolean = current.notNull

  def prepareBallotIsNull: Boolean = prepare.isNull
  def prepareBallotNotNull: Boolean = prepare.notNull

  def preparePrimeBallotIsNull: Boolean = preparePrime.isNull
  def preparePrimeBallotNotNull: Boolean = preparePrime.notNull

  def highBallotIsNull: Boolean = high.isNull
  def highBallotNotNull: Boolean = high.notNull

  def commitBallotIsNull: Boolean = commit.isNull
  def commitBallotNotNull: Boolean = commit.notNull

  def isExternalizePhrase: Boolean = phase.isExternalize
  def notExternalizePhrase: Boolean = ! phase.isExternalize
}

object BallotTracker {
  sealed trait Phase extends Ordered[Phase] {
    def is(name: String): Boolean

    def isExternalize: Boolean = is("Externalized")
    def notExternalize: Boolean = !isExternalize
  }
  object Phase {
    private final case class _Phase(name: String, ord: Int) extends Phase {
      override def compare(that: Phase): Int = this.ord - that.asInstanceOf[_Phase].ord
      override def toString: String           = name

      override def is(name: String): Boolean = this.name == name
    }
    val Prepare: Phase     = _Phase("Prepare", 1)
    val Confirm: Phase     = _Phase("Confirm", 2)
    val Externalize: Phase = _Phase("Externalize", 3)
  }

}
