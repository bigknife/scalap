package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.EnvelopeService
import bigknife.scalap.ast.types._

class EnvelopeServiceHandler extends EnvelopeService.Handler[Stack] {
  override def verifyEnvelopeSignature[M <: Message](envelope: Envelope[M]): Stack[Boolean] = Stack {
    setting =>
      setting.connect.verifySignature(envelope)
  }

  override def validateValue(value: Value): Stack[Boolean] = Stack { setting =>
    // ignore MayBeValid now.
    setting.connect.validateValue(value) == Value.Validity.FullyValidated
  }
}

object EnvelopeServiceHandler {
  private val _instance: EnvelopeServiceHandler = new EnvelopeServiceHandler

  trait Implicits {
    implicit val envelopeServiceHandler: EnvelopeServiceHandler = _instance
  }
}
