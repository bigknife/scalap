package bigknife.scalap.ast.types

import bigknife.scalap.ast.types.BallotTracker.Phase

/**
  * the state tracker of ballot protocol
  */
case class BallotTracker(
    phase: Phase = Phase.Prepare,
    currentMessageLevel: Int = 0,
    heardFromQuorum: Boolean = false,
    currentBallot: Ballot = Ballot.NullBallot, // b, current ballot that node v is attempting to prepare and commit
    prepared: Ballot = Ballot.NullBallot, // p
    preparedPrime: Ballot = Ballot.NullBallot, // p', p and p' is two highest ballot, and p' is less than and incompatible to p
    highBallot: Ballot = Ballot.NullBallot, // h
    commit: Ballot = Ballot.NullBallot, // c
    latestBallotMessages: Map[Node.ID, BallotMessage] = Map.empty, // M
    lastMessage: Option[BallotMessage] = None,
    lastEmittedMessage: Option[BallotMessage] = None
)
object BallotTracker {

  val Empty: BallotTracker = BallotTracker()

  sealed trait Phase
  object Phase {
    case object Prepare extends Phase {
      override def toString: String = "Prepare"
    }
    case object Confirm extends Phase {
      override def toString: String = "Confirm"
    }
    case object Externalized extends Phase {
      override def toString: String = "Externalized"
    }
  }
}
