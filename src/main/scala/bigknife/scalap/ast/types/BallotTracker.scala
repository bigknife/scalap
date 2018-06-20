package bigknife.scalap.ast.types

import bigknife.scalap.ast.types.BallotTracker.Phrase

/**
  * the state tracker of ballot protocol
  */
case class BallotTracker(
    phrase: Phrase = Phrase.Prepare,
    currentBallot: Option[Ballot] = None, // b, current ballot that node v is attempting to prepare and commit
    prepared: Option[Ballot] = None, // p
    preparedPrime: Option[Ballot] = None, // p', p and p' is two highest ballot, and p' is less than and incompatible to p
    highBallot: Option[Ballot] = None, // h
    commit: Option[Ballot] = None, // c
    latestBallotMessages: Map[Node.ID, BallotMessage] = Map.empty// M
)
object BallotTracker {

  val Empty: BallotTracker = BallotTracker()

  sealed trait Phrase
  object Phrase {
    case object Prepare extends Phrase {
      override def toString: String = "Prepare"
    }
    case object Confirm extends Phrase {
      override def toString: String = "Confirm"
    }
    case object Externalized extends Phrase {
      override def toString: String = "Externalized"
    }
  }
}
