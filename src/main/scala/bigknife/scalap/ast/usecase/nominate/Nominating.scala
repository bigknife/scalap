package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

/** Nomination uses case
  *
  */
trait Nominating[F[_]] extends NominationCore[F] {
  self: NominateHelper[F] with ModelSupport[F] with ConvenienceSupport[F] =>

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

    // if the round is 0, we nominate without any conditions. if round > 0, should check the nomination start is on,
    // if on, go ahead, or do nothing

    // if round nominated by 0 multi times, it's ok

    // find the most high priority nodes as the leaders to follow their votes and accepted
    // find the tracker and update nominate tracker
    // then create an nomination message
    // emit nomination message

    for {
      _         <- logService.debug(s"==== START NOMINATING to $nodeID[$slotIndex] ====", Some("nominate"))
      quorumSet <- nodeStore.getQuorumSet(nodeID)
      _         <- logService.debug(s"Got $quorumSet of nodeID=$nodeID", Some("nominate"))
      tracker   <- nodeStore.getNominateTracker(nodeID, slotIndex)
      ret <- ifM[Boolean](false, _ => !(round > 1 && !tracker.nominationStarted)) { _ =>
        for {
          trackerWithLeaders <- nominateService.findRoundLeaders(tracker,
                                                                 quorumSet,
                                                                 round,
                                                                 slotIndex,
                                                                 previousValue)
          _ <- logService.info(s"found round($round) leaders: ${trackerWithLeaders.roundLeaders}",
                               Some("nominate"))
          trackerResult <- nominateService.nominateNewValuesWithLeaders(trackerWithLeaders,
                                                                        nodeID,
                                                                        valueToNominate)
          _ <- logService.debug(
            s"nominated to $nodeID[$slotIndex], result is ${trackerResult.successful}",
            Some("nominate"))
          msgResult <- self.createNominationMessage(nodeID, slotIndex, quorumSet, trackerResult)
          xTracker  <- self.emitNominationMessage(nodeID, trackerResult.data, msgResult)
          _         <- nodeStore.saveNominateTracker(nodeID, xTracker)
          _ <- logService.debug(
            s"saved nomination tracker voted size = ${xTracker.nomination.voted.size}, " +
              s"accepted size = ${xTracker.nomination.accepted.size}",
            Some("nominate"))

          timeout <- nominateService.timeoutForNextRoundNominating(round)

          _ <- nominateService.triggerNextRoundNominating(nodeID,
                                                          slotIndex,
                                                          round + 1,
                                                          valueToNominate,
                                                          previousValue,
                                                          timeout)
          _ <- logService.info(s"trigger next round(${round + 1}) nominating after $timeout ms",
                               Some("nominate"))
        } yield msgResult.successful

      }
      _ <- logService.debug(s"==== END NOMINATING to $nodeID[$slotIndex] ====", Some("nominate"))
    } yield ret
  }
}
