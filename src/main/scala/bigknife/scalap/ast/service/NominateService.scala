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
  def findRoundLeaders(quorumSet: QuorumSet,
                       round: Int,
                       slotIndex: SlotIndex,
                       previousValue: Value): P[F, Set[NodeID]]

  /**
    * try to nominate new values
    * @param tracker current tracker
    * @param nodeID the node which try to nominate the new value
    * @param tryToNominate value to try to nominate
    * @param leaders the nodeId's quorumset's leaders on current round(tracker.round)
    * @return result, if the tracker updated return (new tracker, true) else (old tracker, false)
    */
  def nominateNewValues(tracker: NominateTracker,
                        nodeID: NodeID,
                        tryToNominate: Value,
                        leaders: Set[NodeID]): P[F, NominateNewValuesResult]

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
    * broadcast envelope to peers according to the conditions:
    * 1. no nomination envelope had been broadcast on the node
    * 2. nomination statement in the envelope is newer than last sent nomination envelope
    * @param envelope envelope with nomination statement
    * @return
    */
  def broadcastEnvelope(tracker: NominateTracker, envelope: Envelope[Message.Nomination]): P[F, NominateTracker]
}
