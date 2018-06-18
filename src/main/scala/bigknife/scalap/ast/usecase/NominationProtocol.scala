package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * nomination protocol of scp
  */
trait NominationProtocol[F[_]] extends BaseProtocol[F] {
  import model._

  /**
    * run nomination protocol
    * @param slot a slot
    * @param message nomination statement
    * @return
    */
  def runNominationProtocol(slot: Slot, message: NominationMessage): SP[F, Result] = {
    import cats.implicits._
    val verify:SP[F, Boolean] = (isSane(message.statement), isNewer(slot, message.statement)).mapN(_ && _)

    val process: SP[F, Result] = for {
      slot0 <- slotService.trackNewNominationMessage(slot, message)
      x <- invalidResult(slot0)
    } yield x

    // process after verified
    for {
      passed <- verify
      result <- if(passed) process else invalidResult(slot)
    } yield result
  }

  /**
    * is the statement sane?
    * @param statement statement
    * @return
    */
  private def isSane(statement: NominationStatement): P[F, Boolean] = ???

  /**
    * is the coming message newer than the latest nomination message sent from the node saved in slot.
    * @param slot slot
    * @param statement coming message
    * @return
    */
  private def isNewer(slot: Slot, statement: NominationStatement): P[F, Boolean] = ???
}
