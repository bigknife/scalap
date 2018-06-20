package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

/**
  * ballot protocol of scp protocol
  */
trait BallotProtocol[F[_]] extends BaseProtocol[F] {
  import model._

  def runBallotProtocol(slot: Slot, message: BallotMessage): SP[F, Result] = ???

  /**
    * bump state
    * @param slot slot
    * @param candidate composite candidate value
    * @return
    */
  def bumpState(slot: Slot, candidate: Value): SP[F, Unit] = {
    for {
      _ <- logService.info(s"//TODO: bump state: $candidate")
    } yield ()
  }
}
