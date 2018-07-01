package bigknife.scalap.ast.types

sealed trait Message {}

object Message {
  sealed trait BallotMessage extends Message

  case class Nomination(
      voted: ValueSet,
      accepted: ValueSet
  ) extends Message

  case class Prepare(
      ballot: Ballot,
      prepare: Ballot,
      preparePrime: Ballot,
      hCounter: Int,
      cCounter: Int
  ) extends BallotMessage

  class NominationBuilder {
    private val voted: collection.mutable.ListBuffer[Value]    = collection.mutable.ListBuffer.empty
    private val accepted: collection.mutable.ListBuffer[Value] = collection.mutable.ListBuffer.empty

    def build(): Nomination = {
      // voted and accepted should be disjoint, if eligible to both, put it into accepted
      val votedList     = voted.toList
      val filteredVoted = votedList.filter(accepted.contains)
      Nomination(ValueSet(filteredVoted: _*), ValueSet(accepted: _*))
    }

    def buildAsMessage(): Message = build()

    def fromNomination(other: Nomination): NominationBuilder = {
      vote(other.voted.toVector: _*)
      accept(other.accepted.toVector: _*)
      this
    }

    def vote(value: Value*): NominationBuilder = {
      voted.append(value: _*)
      this
    }
    def accept(value: Value*): NominationBuilder = {
      accepted.append(value: _*)
      this
    }
  }

  def nominationBuilder(): NominationBuilder = new NominationBuilder

  /////// message ops
  trait Ops {
    implicit final class MessageOps(message: Message) {
      private def isAsc(values: ValueSet): Boolean =
        values.sliding(2).exists {
          case Seq()     => true
          case Seq(_)    => true
          case Seq(l, r) => l <= r
        }
      def isSane: Boolean = message match {
        case Nomination(voted, accepted) =>
          // values of voted and accepted should ordered ASC
          !(voted.isEmpty && accepted.isEmpty) && isAsc(voted) && isAsc(accepted)

        case Prepare(ballot, prepare, preparePrime, hCounter, cCounter) =>
          val cond1 = ballot.notNull
          val cond2 = if (prepare.notNull) {
            prepare <= ballot
          } else true
          val cond3 = if (preparePrime.notNull) {
            prepare.notNull && preparePrime < prepare && (cCounter <= hCounter && hCounter <= ballot.counter)
          } else true
          cond1 && cond2 && cond3
      }
    }
  }
}
