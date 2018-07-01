package bigknife.scalap.ast.types

case class BallotTracker(
    nodeID: NodeID,
    slotIndex: SlotIndex,
    phrase: BallotPhrase, // phi, current phrase
    current: Ballot, // b, current ballot try to prepare and commit
    prepare: Ballot, // p, accepted prepare
    preparePrime: Ballot, // p', accepted preparePrime, less and incompatible to prepare
    high: Ballot, // h, highest confirmed ballot.counter, if is 0, no confirmed
    commit: Ballot, // c, lowest confirmed ballot.counter
    latestBallotEnvelope: Map[NodeID, Envelope[Message.BallotMessage]], // M
    lastSentEnvelope: Option[Envelope[Message.BallotMessage]]
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

  def isExternalizePhrase: Boolean = phrase.isExternalize
}

object BallotTracker {
  sealed trait Phrase extends Ordered[Phrase] {
    def is(name: String): Boolean

    def isExternalize: Boolean = is("Externalized")
  }
  object Phrase {
    private final case class _Phrase(name: String, ord: Int) extends Phrase {
      override def compare(that: Phrase): Int = this.ord - that.asInstanceOf[_Phrase].ord
      override def toString: String           = name

      override def is(name: String): Boolean = this.name == name
    }
    val Prepare: Phrase     = _Phrase("Prepare", 1)
    val Confirm: Phrase     = _Phrase("Confirm", 2)
    val Externalize: Phrase = _Phrase("Externalize", 3)
  }

}
