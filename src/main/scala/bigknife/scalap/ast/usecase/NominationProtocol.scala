package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._

/**
  * nomination protocol of scp
  */
trait NominationProtocol[F[_]] extends BaseProtocol[F] {
  import model._

  /**
    * run nomination protocol
    * @param slot a slot
    * @param statement nomination statement
    * @return
    */
  def runNominationProtocol(slot: Slot, statement: NominationStatement): SP[F, Result] = {

    ???
  }

  /**
    * is the statement sane?
    * @param statement statement
    * @return
    */
  private def isSane(statement: NominationStatement): SP[F, Boolean] = ???

  /**
    * is the coming message newer than the latest nomination message sent from the node saved in slot.
    * @param slot slot
    * @param statement coming message
    * @return
    */
  private def isNewer(slot: Slot, statement: NominationStatement): SP[F, Boolean] = ???
}
