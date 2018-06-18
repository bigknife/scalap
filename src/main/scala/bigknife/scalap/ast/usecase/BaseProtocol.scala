package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * base protocol, provide `model` for sub classes.
  */
trait BaseProtocol[F[_]] {
  // result of running protocol
  type Result = (Slot, MessageState)
  def validResult(slot: Slot): SP[F, Result]   = (slot, Message.State.valid).pureSP[F]
  def invalidResult(slot: Slot): SP[F, Result] = (slot, Message.State.invalid).pureSP[F]

  val model: component.Model[F]
}
