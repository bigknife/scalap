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
      passed <- verifyMessage(message)

      state <- if (!passed) MessageState.invalid.pureSP[F]
      else
        for {
          slot   <- getOrCreateSlot(nodeId, message.statement.slotIndex)
          result <- delegateToProtocol(slot, message)
          _      <- slotStore.saveSlotForNode(nodeId, result._1)
        } yield result._2

    } yield state
  }
}

object SCP {
  def apply[F[_]](implicit M: component.Model[F]): SCP[F] = new SCP[F] {
    override val model: component.Model[F] = M

    /**
      * verify a message in application level.
      *
      * @param message message with statement and signature
      * @return if passed true else false
      */
    override def verifyMessage(message: StatementMessage): SP[F, Boolean] = ???




    /**
      * validate a nomination value's validity
      *
      * @param value value
      * @return
      */
    override def validateNominationValue(value: Value): SP[F, Value.Validity] = ???

    /**
      * try to transforms a value to a fully validted value that the local node would agree to
      *
      * @param slot  slot
      * @param value value(not fully validated)
      * @return
      */
    override def extractValidValue(slot: Slot, value: Value): SP[F, Option[Value]] = ???

    /**
      * emit message to other nodes
      *
      * @param message message
      * @return
      */
    override def emitMessage(message: StatementMessage): SP[F, Unit] = ???
  }
}
