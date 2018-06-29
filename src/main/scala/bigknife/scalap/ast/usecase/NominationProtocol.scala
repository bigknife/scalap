package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.sop._
import bigknife.sop.implicits._

/** Nomination uses case
  *
  */
trait NominationProtocol[F[_]] {
  val model: Model[Model.Op]
  import model._

  /**
    * start a nomination round for a node to vote value at a specified index
    * @param nodeID node id
    * @param slotIndex slot index
    * @param valueToNominate value to nominate
    * @param previousValue the value of the slot[slotIndex -1]
    * @return
    */
  def nominate(nodeID: NodeID,
               slotIndex: SlotIndex,
               valueToNominate: Value,
               previousValue: Value): SP[F, Unit] = {
    // find the most high priority nodes as the leaders to follow their votes and accepted

    // find the tracker and update nominate tracker

    // then create an nomination message

    // emit nomination message

    for {
      quorumSet <- nodeStore.getQuorumSet(nodeID)
      tracker   <- nodeStore.getNominateTracker(nodeID, slotIndex)
      leaders <- nominateService.findRoundLeaders(quorumSet,
                                                  tracker.round,
                                                  slotIndex,
                                                  previousValue)
      result <- nominateService.nominateNewValues(tracker, nodeID, valueToNominate, leaders)

    } yield ()

    ???
  }
}
