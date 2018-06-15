package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._

/**
  * nomination protocol of scp
  */
trait NominationProtocol[F[_]] extends BaseProtocol[F] {

  def runNominationProtocol(slot: Slot, statement: NominationStatement): SP[F, Result]
}
