package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.SlotService

class SlotServiceHandler extends SlotService.Handler[Stack]{

}

object SlotServiceHandler {
  trait Implicits {
    implicit val slotServiceHandler: SlotServiceHandler = new SlotServiceHandler
  }
}
