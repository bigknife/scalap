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
    for {
      passed <- verifyMessage(message)

      state <- if (!passed) MessageState.invalid.pureSP
      else
        for {
          slotOpt <- slotStore.getSlotOfNode(nodeId, message.statement.slotIndex)
          slot <- if (slotOpt.isDefined) slotOpt.get.pureSP
          else
            for {
              s0 <- slotService.createSlot(nodeId, message.statement.slotIndex)
              _  <- slotStore.saveSlotForNode(nodeId, message.statement.slotIndex, s0)
            } yield s0

          result <- message.statement match {
            case x: NominationStatement => runNominationProtocol(slot, x)
            case x: BallotStatement     => runBallotProtocol(slot, x)
          }

          _ <- slotStore.saveSlotForNode(nodeId, message.statement.slotIndex, result._1)
        } yield result._2

    } yield state
  }
}
