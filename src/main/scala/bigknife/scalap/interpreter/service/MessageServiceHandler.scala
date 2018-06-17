package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.MessageService
import bigknife.scalap.ast.types.Message

class MessageServiceHandler extends MessageService.Handler[Stack] {
  override def isSaneNominationStatement(statement: Message.NominationStatement): Stack[Boolean] =
    Stack {
      ???
    }
}

object MessageServiceHandler {
  trait Implicits {
    implicit val messageServiceHandler: MessageServiceHandler = new MessageServiceHandler
  }
}
