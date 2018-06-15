package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._

/**
  * base protocol, provide `model` for sub classes.
  */
trait BaseProtocol[F[_]] {
  // result of running protocol
  type Result = (Slot, MessageState)

  val model: component.Model[F]
}
