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
    * @param round current round
    * @param valueToNominate value to nominate
    * @param previousValue the value of the slot[slotIndex -1]
    * @return
    */
  def nominate(nodeID: NodeID,
               slotIndex: SlotIndex,
               round: Int,
               valueToNominate: Value,
               previousValue: Value): SP[F, Boolean] = {
    // find the most high priority nodes as the leaders to follow their votes and accepted
    // find the tracker and update nominate tracker
    // then create an nomination message
    // emit nomination message

    for {
      _         <- logService.debug(s"start to nominate $valueToNominate to $nodeID[$slotIndex]", Some("nomination"))
      quorumSet <- nodeStore.getQuorumSet(nodeID)
      _         <- logService.debug(s"Got $quorumSet of nodeID=$nodeID", Some("nomination"))
      tracker   <- nodeStore.getNominateTracker(nodeID, slotIndex)
      trackerWithLeaders <- nominateService.findRoundLeaders(tracker,
                                                             quorumSet,
                                                             round,
                                                             slotIndex,
                                                             previousValue)
      _ <- logService.info(s"found round($round) leaders: ${trackerWithLeaders.roundLeaders}", Some("nomination"))
      trackerResult <- nominateService.nominateNewValuesWithLeaders(trackerWithLeaders,
                                                                    nodeID,
                                                                    valueToNominate)
      _ <- logService.debug(
        s"nominated  $valueToNominate to $nodeID[$slotIndex], result is $trackerResult", Some("nomination"))
      msgResult <- self.createNominationMessage(nodeID, slotIndex, quorumSet, trackerResult)
      xTracker  <- self.emitNominationMessage(nodeID, trackerResult.data, msgResult)
      _         <- logService.info(s"emitted nomination message: $xTracker", Some("nomination"))
      _         <- nodeStore.saveNominateTracker(nodeID, xTracker)
      _         <- logService.debug(s"saved nomination tracker: $xTracker", Some("nomination"))
    } yield msgResult.successful
  }
}
