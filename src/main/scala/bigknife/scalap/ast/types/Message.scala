package bigknife.scalap.ast.types

sealed trait Message {}

object Message {
  sealed trait BallotMessage extends Message {
    def order: Int

    def isPrepare: Boolean
    def noPrepare: Boolean = !isPrepare

    def isCommit: Boolean
    def notCommit: Boolean = !isCommit

    def isExternalize: Boolean
    def noExternalize: Boolean = !isExternalize
  }

  case class Nomination(
      voted: ValueSet,
      accepted: ValueSet
  ) extends Message {
    def isEmpty: Boolean  = voted.isEmpty && accepted.isEmpty
    def notEmpty: Boolean = !isEmpty
  }

  case class Prepare(
      ballot: Ballot,
      prepared: Ballot,
      preparedPrime: Ballot,
      hCounter: Int,
      cCounter: Int
  ) extends BallotMessage {
    override def order: Int = 1

    override def isPrepare: Boolean = true

    override def isCommit: Boolean = false

    override def isExternalize: Boolean = false
  }

  /** also: Commit */
  case class Commit(
      ballot: Ballot,
      preparedCounter: Int,
      hCounter: Int,
      cCounter: Int
  ) extends BallotMessage {
    override def order: Int = 2

    override def isPrepare: Boolean     = false
    override def isCommit: Boolean      = true
    override def isExternalize: Boolean = false
  }

  case class Externalize(
      commit: Ballot,
      hCounter: Int
  ) extends BallotMessage {
    override def order: Int = 3

    override def isPrepare: Boolean = false

    override def isCommit: Boolean = false

    override def isExternalize: Boolean = true
  }

  class NominationBuilder {
    private val voted: collection.mutable.ListBuffer[Value]    = collection.mutable.ListBuffer.empty
    private val accepted: collection.mutable.ListBuffer[Value] = collection.mutable.ListBuffer.empty

    def build(): Nomination = {
      // voted and accepted should be disjoint, if eligible to both, put it into accepted
      //val votedList     = voted.toList
      //val filteredVoted = votedList.filter(x => !accepted.contains(x))
      Nomination(ValueSet(voted: _*), ValueSet(accepted: _*))
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

  // null smart constructor
  def nullPrepare: Message.Prepare = Message.Prepare(
    Ballot.Null,
    Ballot.Null,
    Ballot.Null,
    0,
    0
  )
  def nullCommit: Message.Commit           = Message.Commit(Ballot.Null, 0, 0, 0)
  def nullExternalize: Message.Externalize = Message.Externalize(Ballot.Null, 0)

  /////// message ops
  trait Ops {
    implicit final class MessageOps(message: Message) {
      private def isAsc(values: ValueSet): Boolean = {
        val s = values.sliding(2)
        if (s.isEmpty) true
        else
          s.exists {
            case Seq()     => true
            case Seq(_)    => true
            case Seq(l, r) => l <= r
          }
      }
      /**
        *@param self if the message is sent by self
        */
      def isSane(self: Boolean): Boolean = message match {
        case x @ Nomination(voted, accepted) =>
          // values of voted and accepted should ordered ASC
          x.notEmpty && isAsc(voted) && isAsc(accepted)

        case Prepare(ballot, prepare, preparePrime, hCounter, cCounter) =>
          val cond1 = self || ballot.notNull
          val cond2 = if (prepare.notNull) {
            prepare <= ballot
          } else true
          val cond3 = if (preparePrime.notNull) {
            prepare.notNull && preparePrime < prepare && (cCounter <= hCounter && hCounter <= ballot.counter)
          } else true
          cond1 && cond2 && cond3

        case Commit(ballot, _, hCounter, cCounter) =>
          // c <= h <= b
          val cond1 = ballot.counter > 0
          val cond2 = hCounter <= ballot.counter
          val cond3 = cCounter <= hCounter
          cond1 && cond2 && cond3

        case Externalize(commit, hCounter) =>
          commit.counter > 0 && hCounter >= commit.counter
      }

      def notSane(self: Boolean): Boolean = !isSane(self)
    }
  }
}
