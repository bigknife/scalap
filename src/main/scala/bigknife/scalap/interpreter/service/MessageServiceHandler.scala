package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.MessageService

class MessageServiceHandler extends MessageService.Handler[Stack] {}

object MessageServiceHandler {
  trait Implicits {
    implicit val messageServiceHandler: MessageServiceHandler = new MessageServiceHandler
  }
}
