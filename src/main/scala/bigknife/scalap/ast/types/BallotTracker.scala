package bigknife.scalap.ast.types

case class BallotTracker(
    nodeID: NodeID,
    slotIndex: SlotIndex,
    phase: BallotPhase, // phi, current phrase
    current: Ballot, // b, current ballot try to prepare and commit
    prepared: Ballot, // p, accepted prepare
    preparedPrime: Ballot, // p', accepted preparePrime, less and incompatible to prepare
    high: Ballot, // h, highest confirmed ballot.counter, if is 0, no confirmed
    commit: Ballot, // c, lowest confirmed ballot.counter
    latestBallotEnvelope: Map[NodeID, BallotEnvelope[Message.BallotMessage]], // M
    lastGenEnvelope: Option[BallotEnvelope[Message.BallotMessage]],
    lastEmitEnvelope: Option[BallotEnvelope[Message.BallotMessage]],
    heardFromQuorum: Boolean
) {
  def currentBallotIsNull: Boolean  = current.isNull
  def currentBallotNotNull: Boolean = current.notNull

  def preparedBallotIsNull: Boolean  = prepared.isNull
  def preparedBallotNotNull: Boolean = prepared.notNull

  def preparedPrimeBallotIsNull: Boolean  = preparedPrime.isNull
  def preparedPrimeBallotNotNull: Boolean = preparedPrime.notNull

  def highBallotIsNull: Boolean  = high.isNull
  def highBallotNotNull: Boolean = high.notNull

  def commitBallotIsNull: Boolean  = commit.isNull
  def commitBallotNotNull: Boolean = commit.notNull

  def isExternalizePhase: Boolean  = phase.isExternalize
  def notExternalizePhase: Boolean = !phase.isExternalize

  def isCommitPhase: Boolean  = phase.isCommit
  def notCommitPhase: Boolean = !phase.isCommit

  def isPreparePhase: Boolean  = phase.isPrepare
  def notPreparePhase: Boolean = !phase.isPrepare
}

object BallotTracker {
  sealed trait Phase extends Ordered[Phase] {
    def is(name: String): Boolean

    def isExternalize: Boolean  = is("Externalized")
    def notExternalize: Boolean = !isExternalize

    def isCommit: Boolean  = is("Commit")
    def notCommit: Boolean = !isCommit

    def isPrepare: Boolean  = is("Prepare")
    def notPrepare: Boolean = !isPrepare
  }
  object Phase {
    private final case class _Phase(name: String, ord: Int) extends Phase {
      override def compare(that: Phase): Int = this.ord - that.asInstanceOf[_Phase].ord
      override def toString: String          = name

      override def is(name: String): Boolean = this.name == name
    }
    val Prepare: Phase     = _Phase("Prepare", 1)
    val Commit: Phase      = _Phase("Commit", 2)
    val Externalize: Phase = _Phase("Externalize", 3)
  }

}
