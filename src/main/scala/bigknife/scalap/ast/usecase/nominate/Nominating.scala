package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.ModelSupport
import bigknife.sop._
import bigknife.sop.implicits._

/** Nomination uses case
  *
  */
trait Nominating[F[_]] extends NominationCore[F] {
  self: NominateHelper[F] with ModelSupport[F] =>

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
               previousValue: Value): SP[F, Boolean] = {
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
      trackerResult <- nominateService.nominateNewValuesWithLeaders(tracker, nodeID, valueToNominate, leaders)
      msgResult     <- self.createNominationMessage(nodeID, slotIndex, quorumSet, trackerResult)
      xTracker      <- self.emitNominationMessage(nodeID, trackerResult.data, msgResult)
      _             <- nodeStore.saveNominateTracker(nodeID, xTracker)
    } yield msgResult.successful
  }
}
