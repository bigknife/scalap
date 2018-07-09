package bigknife.scalap.ast.service

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait EnvelopeService[F[_]] {

  /**
    * verify envelope's signature
    * @param envelope envelope
    * @return
    */
  def verifyEnvelopeSignature[M <: Message](
      envelope: Envelope[M]): P[F, Boolean]

  /**
    * validate the value is legal on the application level
    * @param value value
    * @return
    */
  def validateValue(value: Value): P[F, Boolean]
}
