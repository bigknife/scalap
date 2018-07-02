package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.EnvelopeService
import bigknife.scalap.ast.types.{Envelope, Message}

class EnvelopeServiceHandler extends EnvelopeService.Handler[Stack] {
  override def verifyEnvelopeSignature[M <: Message](envelope: Envelope[M]): Stack[Boolean] = Stack {
    setting =>
      setting.connect.verifySignature(envelope)
  }
}

object EnvelopeServiceHandler {
  private val _instance: EnvelopeServiceHandler = new EnvelopeServiceHandler

  trait Implicits {
    implicit val envelopeServiceHandler: EnvelopeServiceHandler = _instance
  }
}
