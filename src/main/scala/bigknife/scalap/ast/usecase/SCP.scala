package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

trait SCP[F[_]] extends BaseProtocol[F] with NominationProtocol[F] with BallotProtocol[F] {

  import model._

  /**
    * verify a message in application level.
    * @param message message with statement and signature
    * @return if passed true else false
    */
  def verifyMessage(message: StatementMessage): SP[F, Boolean]

  /**
    * handle a message for a scp node
    * @param nodeId scp node
    * @param message coming message
    * @return
    */
  final def handleMessage(nodeId: Node.ID, message: StatementMessage): SP[F, MessageState] = {

    def getOrCreateSlot(nodeId: Node.ID, slotIndex: Long): SP[F, Slot] =
      for {
        slotOpt <- slotStore.getSlotOfNode(nodeId, slotIndex)
        slot <- if (slotOpt.isDefined) slotOpt.get.pureSP[F]
        else
          for {
            s0 <- slotService.createSlot(nodeId, message.statement.slotIndex)
            _  <- slotStore.saveSlotForNode(nodeId, message.statement.slotIndex, s0)
          } yield s0
      } yield slot

    def delegateToProtocol(slot: Slot, message: StatementMessage): SP[F, Result] = {
      message.statement match {
        case x: NominationStatement => runNominationProtocol(slot, x)
        case x: BallotStatement     => runBallotProtocol(slot, x)
      }
    }
    for {
      passed <- verifyMessage(message)

      state <- if (!passed) MessageState.invalid.pureSP[F]
      else
        for {
          slot   <- getOrCreateSlot(nodeId, message.statement.slotIndex)
          result <- delegateToProtocol(slot, message)
        } yield result._2

    } yield state
  }
}

object SCP {
  def apply[F[_]](implicit M: component.Model[F]): SCP[F] = new SCP[F] {

    /**
      * verify a message in application level.
      *
      * @param message message with statement and signature
      * @return if passed true else false
      */
    override def verifyMessage(message: StatementMessage): SP[F, Boolean] = ???

    override def runBallotProtocol(slot: Slot,
                                   statement: BallotStatement): SP[F, (Slot, MessageState)] = ???

    override val model: component.Model[F] = M
  }
}
