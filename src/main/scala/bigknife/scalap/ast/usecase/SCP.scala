package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

trait SCP[F[_]] extends BaseProtocol[F] with NominationProtocol[F] with BallotProtocol[F] {

  import model._



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
            _ <- logService.info(s"create a new slot for Node.ID($nodeId)#SlotIndex(${message.statement.slotIndex})")
            _  <- slotStore.saveSlotForNode(nodeId, s0)
          } yield s0
      } yield slot

    def delegateToProtocol(slot: Slot, message: StatementMessage): SP[F, Result] = {
      message.statement match {
        case _: Message.NominationStatement =>
          runNominationProtocol(slot, message.asInstanceOf[NominationMessage])
        case _: Message.BallotStatement =>
          runBallotProtocol(slot, message.asInstanceOf[BallotMessage])
      }
    }

    // put together
    for {
      passed <- applicationExtension.verifyMessage(message)
      _      <- logService.info(s"verify application message: $passed")
      state <- if (!passed) MessageState.invalid.pureSP[F]
      else
        for {
          slot   <- getOrCreateSlot(nodeId, message.statement.slotIndex)
          _      <- logService.info(s"current slot: $slot")
          result <- delegateToProtocol(slot, message)
          _      <- logService.info(s"after protocol: $result")
          _      <- slotStore.saveSlotForNode(nodeId, result._1)
        } yield result._2

    } yield state
  }

  final def getSlot(nodeId: Node.ID, slotIndex: Long): SP[F, Option[Slot]] =
    slotStore.getSlotOfNode(nodeId, slotIndex)

  final def saveQuorumSet(quorumSet: QuorumSet): SP[F, Unit] = for {
    hash <- quorumSetService.hashOfQuorumSet(quorumSet)
    _ <- quorumSetStore.saveQuorumSet(hash, quorumSet)
  } yield ()
}
