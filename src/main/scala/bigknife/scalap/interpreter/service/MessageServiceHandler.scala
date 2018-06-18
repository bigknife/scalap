package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.MessageService
import bigknife.scalap.ast.types.{Message, Value}

class MessageServiceHandler extends MessageService.Handler[Stack] {
  override def isSaneNominationStatement(statement: Message.NominationStatement): Stack[Boolean] =
    Stack {
      def isAsc(values: Vector[Value]): Boolean =
        !values.sliding(2).exists {
          case Vector(l, r) => l > r
        }

      statement match {
        case x: Message.Nominate =>
          x.votes.nonEmpty && x.accepted.nonEmpty &&
            isAsc(x.votes) && isAsc(x.accepted)
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
              if (y.votes.containsSlice(x.votes) &&
                  y.accepted.containsSlice(x.accepted)) y
              else x
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
