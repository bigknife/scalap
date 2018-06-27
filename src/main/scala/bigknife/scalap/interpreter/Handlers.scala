package bigknife.scalap.interpreter

import bigknife.scalap.ast.service.ApplicationExtension
import bigknife.scalap.interpreter.store._
import bigknife.scalap.interpreter.service._

trait Handlers
    extends MessageServiceHandler.Implicits
    with SlotServiceHandler.Implicits
    with SlotStoreHandler.Implicits
    with QuorumSetServiceHandler.Implicits
    with QuorumSetStoreHandler.Implicits
    with LogServiceHandler.Implicits
    with NodeServiceHandler.Implicits
    with NodeStoreHandler.Implicits {

  implicit val applicationExtension: ApplicationExtension.Handler[Stack]
}
