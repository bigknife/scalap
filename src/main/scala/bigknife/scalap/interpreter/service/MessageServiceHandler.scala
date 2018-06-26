package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.MessageService
import bigknife.scalap.ast.types
import bigknife.scalap.ast.types.BallotTracker.Phase
import bigknife.scalap.ast.types._
import org.slf4j.{Logger, LoggerFactory}

class MessageServiceHandler extends MessageService.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def isSaneNominationStatement(statement: Message.NominationStatement): Stack[Boolean] =
    Stack {
      def isAsc(values: Vector[Value]): Boolean =
        !values.sliding(2).exists {
          case Vector() => true
          case Vector(l)    => false
          case Vector(l, r) => l > r
        }

      statement match {
        case x: Message.Nominate =>
          val sane = (x.votes.length + x.accepted.length) != 0 &&
            isAsc(x.votes) && isAsc(x.accepted)
          logger.debug(s"nomination statement is sane? $sane")
          sane
      }
    }

  override def newerNominationStatement(
      st1: Message.NominationStatement,
      st2: Message.NominationStatement): Stack[Message.NominationStatement] = Stack {
    if (st1 == st2) st1
    else {
      // if st2 is sub slice of st1, st2 is newer than st1.
      st1 match {
        case x: Message.Nominate =>
          st2 match {
            case y: Message.Nominate =>
              if ((y.votes.length > x.votes.length) && y.votes.containsSlice(x.votes) &&
                  (y.accepted.length > x.accepted.length) && y.accepted.containsSlice(x.accepted)) y
              else x
          }
      }
    }
  }

  override def createNominationMessage(slot: Slot, quorumSetHash: Hash): Stack[NominationMessage] =
    Stack {
      // todo sign the message
      val nominate = Message.Nominate(
        nodeId = slot.nodeId,
        slotIndex = slot.index,
        votes = slot.nominateTracker.voted,
        accepted = slot.nominateTracker.accepted,
        quorumSetHash = quorumSetHash
      )

      Message(nominate, Signature(Array.emptyByteArray))
    }

  override def firstNominationStatementIsNewer(st1: types.NominationStatement,
                                               st2: types.NominationStatement): Stack[Boolean] =
    Stack {
      if (st1 == st2) false
      else {
        st1 match {
          case x: Message.Nominate =>
            st2 match {
              case y: Message.Nominate =>
                (x.votes.length > y.votes.length) && x.votes.containsSlice(y.votes) &&
                  (x.accepted.length > y.accepted.length) && x.accepted.containsSlice(y.accepted)
            }
        }
      }
    }

  override def firstBallotStatementIsNewer(st1: BallotStatement,
                                           st2: BallotStatement): Stack[Boolean] = Stack {
    // prepare < confirm < externalize
    if (st1.order == st2.order) {
      // if both a externalize, false, impossible
      (st1, st2) match {
        case (_: BallotExternalizeStatement, _: BallotExternalizeStatement) => false
        case (x: BallotConfirmStatement, y: BallotConfirmStatement)         =>
          // sorted by (b, p, p', h) (p' = 0 implicitly)
          compareBallotOpt(y.ballot, x.ballot) match {
            case x0 if x0 < 0 => true
            case x0 if x0 > 0 => false
            case _ =>
              if (x.nPrepared == y.nPrepared) y.nH > x.nH
              else y.nPrepared < x.nPrepared
          }
        case (x: BallotPrepareStatement, y: BallotPrepareStatement) =>
          compareBallotOpt(y.ballot, x.ballot) match {
            case x0 if x0 < 0 => true
            case x0 if x0 > 0 => false
            case _ =>
              compareBallotOpt(y.preparedPrime, x.preparedPrime) match {
                case x1 if x1 < 0 => true
                case x1 if x1 > 0 => false
                case _            => y.nH < x.nH
              }
          }
        case _ => false // if the order is equal, this should not happen
      }

    } else st2.order > st2.order
  }

  override def isSaneBallotStatement(statement: BallotStatement, self: Boolean): Stack[Boolean] =
    Stack {
      statement match {
        case x: Message.Prepare =>
          // b =/ 0
          val bGt0 = x.ballot != Ballot.NullBallot

          // p and p' should be incompatible, and p' < p
          val pp = (x.preparedPrime.isZero || x.prepared.isZero) || x.preparedPrime <= x.prepared && x.preparedPrime.incompatible(x.prepared)

          val h = x.nH == 0 || x.prepared.counter >= x.nH

          // c != 0 -> c <= h <= b
          val c = x.nC == 0 || (x.nH != 0 && x.ballot.counter >= x.nH && x.nH >= x.nC)

          bGt0 && pp && h && c

        case x: Message.Confirm =>
          x.ballot.exists(_.counter > 0) && (x.nH <= x.ballot
            .map(_.counter)
            .getOrElse(0)) && (x.nCommit <= x.nH)

        case x: Message.Externalize =>
          x.commit.counter > 0 && (x.nH >= x.commit.counter)
      }
    }

  override def getWorkingBallot(st: BallotStatement): Stack[Ballot] = Stack {
    st match {
      case x: BallotPrepareStatement     => x.ballot
      case x: BallotConfirmStatement     => Ballot(x.nCommit, x.ballot.get.value)
      case x: BallotExternalizeStatement => x.commit
    }
  }

  override def getPreparedCandidates(slot: Slot, st: BallotStatement): Stack[Vector[Ballot]] =
    Stack {
      val hintBallots = st match {
        case x: BallotPrepareStatement =>
          (if (x.ballot.isNotZero) Vector(x.ballot) else Vector()) ++
            (if (x.prepared.isNotZero) Vector(x.prepared) else Vector()) ++
            (if (x.preparedPrime.isNotZero) Vector(x.preparedPrime) else Vector())
          //Vector(x.ballot, x.prepared, x.preparedPrime)
        case x: BallotConfirmStatement =>
          Vector(Ballot(x.nPrepared, x.ballot.get.value), Ballot(Int.MaxValue, x.ballot.get.value))
        case x: BallotExternalizeStatement =>
          Vector(Ballot(Int.MaxValue, x.commit.value))
      }
      val sorted = hintBallots.sorted
      // from right(top)
      sorted.foldRight(Vector.empty[Ballot]) { (n, acc) =>
        slot.ballotTracker.latestBallotMessages.foldLeft(acc) {
          case (_acc, (_, message)) =>
            message.statement match {
              case x: BallotPrepareStatement =>
                val lb: scala.collection.mutable.ListBuffer[Ballot] =
                  scala.collection.mutable.ListBuffer.empty
                if (areBallotsLessAndCompatible(x.ballot, n)) lb.append(x.ballot) else ()
                if (x.prepared != Ballot.NullBallot && areBallotsLessAndCompatible(x.prepared, n))
                  lb.append(x.prepared)
                else ()
                if (x.preparedPrime != Ballot.NullBallot && areBallotsLessAndCompatible(
                      x.preparedPrime,
                      n))
                  lb.append(x.preparedPrime)
                else ()
                _acc ++ lb.toVector

              case x: BallotConfirmStatement =>
                if (n.compatible(x.ballot.get)) {
                  val x1: Vector[Ballot] = _acc :+ n
                  val x2: Vector[Ballot] = if (x.nPrepared < n.counter) {
                    Vector(Ballot(x.nPrepared, n.value))
                  } else Vector.empty[Ballot]
                  x1 ++ x2
                } else _acc

              case x: BallotExternalizeStatement =>
                if (x.commit.compatible(n)) _acc :+ n
                else _acc
            }
        }
      }
    }

  override def createBallotMessage(slot: Slot, quorumSetHash: Hash): Stack[BallotMessage] = Stack {
    //todo sign the message
    slot.ballotTracker.phase match {
      case Phase.Prepare =>
        Message(
          Message.Prepare(
            slot.nodeId,
            slot.index,
            quorumSetHash,
            slot.ballotTracker.currentBallot,
            slot.ballotTracker.prepared,
            slot.ballotTracker.preparedPrime,
            slot.ballotTracker.commit.counter,
            slot.ballotTracker.highBallot.counter
          ),
          Signature.Empty
        )
      case Phase.Confirm =>
        Message(
          Message.Confirm(
            slot.nodeId,
            slot.index,
            quorumSetHash,
            Some(slot.ballotTracker.currentBallot),
            slot.ballotTracker.prepared.counter,
            slot.ballotTracker.commit.counter,
            slot.ballotTracker.highBallot.counter
          ),
          Signature.Empty
        )
      case Phase.Externalized =>
        Message(
          Message.Externalize(slot.nodeId,
                              slot.index,
                              quorumSetHash,
                              slot.ballotTracker.commit,
                              slot.ballotTracker.highBallot.counter),
          Signature.Empty
        )

    }
  }

  private def areBallotsLessAndCompatible(b1: Ballot, b2: Ballot): Boolean =
    compareBallotOpt(Some(b1), Some(b2)) <= 0 && b1.compatible(b2)

  /**
    * compare two optional ballot
    *
    * @param st1 first
    * @param st2 second
    * @return st1 > st2 => 1, st1 < st2 => -1 st1 == st2 => 0
    */
  private def compareBallotOpt(st1: Option[Ballot], st2: Option[Ballot]): Int = {
    if (st1.isDefined && st2.isEmpty) 1
    else if (st1.isEmpty && st2.isDefined) -1
    else if (st1.isEmpty && st2.isEmpty) 0
    else { // both defined
      val s1 = st1.get
      val s2 = st2.get
      if (s1.counter > s2.counter) 1
      else if (s1.counter < s2.counter) -1
      else if (s1.value > s2.value) 1
      else if (s1.value < s2.value) -1
      else 0
    }
  }

  private def compareBallotOpt(s1: Ballot, s2: Ballot): Int = {
    if (s1.counter > s2.counter) 1
    else if (s1.counter < s2.counter) -1
    else if (s1.value > s2.value) 1
    else if (s1.value < s2.value) -1
    else 0

  }
}

object MessageServiceHandler {
  trait Implicits {
    implicit val messageServiceHandler: MessageServiceHandler = new MessageServiceHandler
  }
}
