package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

/**
  * ballot protocol of scp protocol
  */
trait BallotProtocol[F[_]] extends BaseProtocol[F] {
  def runBallotProtocol(slot: Slot, statement: BallotStatement): SP[F, Result]
}
