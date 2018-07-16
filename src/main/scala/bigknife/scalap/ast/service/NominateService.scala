package bigknife.scalap.ast.service

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait NominateService[F[_]] {

  /**
    * find the leaders in the quorum set, then we will follow them
    * to incorporate there votes and accepted
    * @param round current nominating round
    * @param quorumSet quorum set
    * @param slotIndex slot index
    * @param previousValue the value of last slot (slotIndex - 1)
    * @return leaders node id.
    */
  def findRoundLeaders(tracker: NominateTracker,
                       quorumSet: QuorumSet,
                       round: Int,
                       slotIndex: SlotIndex,
                       previousValue: Value): P[F, NominateTracker]

  /**
    * try to nominate new values
    * @param tracker current tracker
    * @param nodeID the node which try to nominate the new value
    * @param tryToNominate value to try to nominate
    * @return result, if the tracker updated return (new tracker, true) else (old tracker, false)
    */
  def nominateNewValuesWithLeaders(tracker: NominateTracker,
                                   nodeID: NodeID,
                                   tryToNominate: Value): P[F, NominateNewValuesResult]

  /**
    * resolve value from nomination message
    * @param tracker nomination tracker
    * @param nom nomination message
    * @return
    */
  def getNewValueFromNomination(tracker: NominateTracker,
                                nom: Message.Nomination): P[F, Option[Value]]

  /**
    * create a nomination envelope
    * @param nodeID node, who create the envelope, use the node's public key could verify the envelope
    * @param nomination nomination message
    * @return
    */
  def createNominationEnvelope(nodeID: NodeID,
                               slotIndex: SlotIndex,
                               quorumSet: QuorumSet,
                               nomination: Message.Nomination): P[F, Envelope[Message.Nomination]]

  /**
    * need to broadcast an envelope ?
    * 1. no nomination envelope had been broadcast on the node
    * 2. nomination statement in the envelope is newer than last sent nomination envelope
    *
    * @param tracker tracker
    * @param envelope envelope to emit
    * @return
    */
  def needBroadcastEnvelope(tracker: NominateTracker,
                            envelope: Envelope[Message.Nomination]): P[F, Boolean]
  /**
    * just broadcast to peers
    * @param envelope envelope with nomination statement
    * @return
    */
  def broadcastEnvelope(tracker: NominateTracker,
                        envelope: Envelope[Message.Nomination]): P[F, NominateTracker]

  /**
    * record envelope for the tracker
    * @param tracker nomination tracker
    * @param envelope envelope
    * @return
    */
  def recordEnvelope(tracker: NominateTracker,
                     envelope: Envelope[Message.Nomination]): P[F, NominateTracker]

  /**
    * combine value sets to one value
    * @param valueSet values
    * @return
    */
  def combineValues(valueSet: ValueSet): P[F, Value]

  /**
    * stop nomination, set nominating flat to false
    * @param tracker
    * @return
    */
  def stopNomination(tracker: NominateTracker): P[F, NominateTracker]

  /**
    * timeout for next round nominating to wait.
    * @param currentRound current round
    * @return timeout for (currentRound + 1), in millis seconds.
    */
  def timeoutForNextRoundNominating(currentRound: Int): P[F, Long]

  /**
    * trigger next round nominating
    * @param nodeID node id
    * @param slotIndex slotIndex
    * @param nextRound next round number
    * @param valueToNominate value to nominate
    * @param previousValue previous value
    * @param afterMilliSeconds after millis seconds
    * @return
    */
  def triggerNextRoundNominating(nodeID: NodeID,
                                 slotIndex: SlotIndex,
                                 nextRound: Int,
                                 valueToNominate: Value,
                                 previousValue: Value,
                                 afterMilliSeconds: Long): P[F, Unit]
}
