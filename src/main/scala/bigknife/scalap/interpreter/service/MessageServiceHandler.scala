package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.MessageService
import bigknife.scalap.ast.types
import bigknife.scalap.ast.types._
import org.slf4j.{Logger, LoggerFactory}

class MessageServiceHandler extends MessageService.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def isSaneNominationStatement(statement: Message.NominationStatement): Stack[Boolean] =
    Stack {
      def isAsc(values: Vector[Value]): Boolean =
        !values.sliding(2).exists {
          case Vector(l) => false
          case Vector(l, r) => l > r
        }

      statement match {
        case x: Message.Nominate =>
          val sane = x.votes.nonEmpty && x.accepted.nonEmpty &&
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
              if  ( (y.votes.length > x.votes.length) && y.votes.containsSlice(x.votes) &&
                (y.accepted.length > x.accepted.length) && y.accepted.containsSlice(x.accepted)) y
              else x
          }
      }
    }
  }

  override def createNominationMessage(slot: Slot, quorumSetHash: Hash): Stack[NominationMessage] = Stack {
    val nominate = Message.Nominate(
      nodeId = slot.nodeId,
      slotIndex = slot.index,
      votes = slot.nominateTracker.voted,
      accepted = slot.nominateTracker.accepted,
      quorumSetHash = quorumSetHash
    )

    Message(nominate, Signature(Array.emptyByteArray))
  }

  override def firstNominationStatementIsNewer(st1: types.NominationStatement, st2: types.NominationStatement): Stack[Boolean] = Stack {
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
}

object MessageServiceHandler {
  trait Implicits {
    implicit val messageServiceHandler: MessageServiceHandler = new MessageServiceHandler
  }
}
