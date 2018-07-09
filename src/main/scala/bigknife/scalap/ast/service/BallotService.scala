package bigknife.scalap.ast
package service

import types._

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait BallotService[F[_]] {

  /**
    * create an appropriate ballot.
    * if tracker's high ballot is not empty, the coming value is ignored
    * @param tracker tracker
    * @param value coming value
    * @return
    */
  def newBallot(tracker: BallotTracker, value: Value, counter: Int): P[F, Ballot]

  /**
    * record envelope to tracker
    * @param tracker ballot tracker
    * @param envelope envelope
    * @return
    */
  def recordEnvelope[M <: BallotMessage](tracker: BallotTracker,
                                         envelope: BallotEnvelope[M]): P[F, BallotTracker]

  /**
    * compute timeout in millisecond
    * @param tracker tracer.
    * @return
    */
  def computeBallotTimeout(tracker: BallotTracker): P[F, Long]

  /**
    * start ballot timer
    * @param nodeID node id
    * @param slotIndex slot index
    */
  def startBallotTimer(nodeID: NodeID, slotIndex: SlotIndex, timeout: Long): P[F, Unit]

  /**
    * stop ballot timer
    * @param nodeID node id
    * @param slotIndex slot index
    * @return
    */
  def stopBallotTimer(nodeID: NodeID, slotIndex: SlotIndex): P[F, Unit]

  /**
    * try to set accept prepare ballot to a tracker
    * @param tracker tracker
    * @param ballot ballot
    * @return
    */
  def setPreparedAccepted(tracker: BallotTracker, ballot: Ballot): P[F, Delta[BallotTracker]]

  /**
    * set prepared committed.
    * @param tracker tracker
    * @param commitBallot to commit, maybe a Ballot.Null (to be ignored)
    * @param highBallot high ballot
    * @return
    */
  def setPreparedCommitted(tracker: BallotTracker, commitBallot: Ballot, highBallot: Ballot): P[F, Delta[BallotTracker]]

  /**
    * check if we also need to clear commit of tracker
    * @param tracker tracker
    * @return
    */
  def clearCommitIfNeeded(tracker: BallotTracker): P[F, Delta[BallotTracker]]

  /**
    * create ballot envelope
    * @param tracker tracker
    * @param quorumSet quorum set
    * @return
    */
  def createBallotEnvelope(tracker: BallotTracker,
                           quorumSet: QuorumSet): P[F, BallotEnvelope[BallotMessage]]

  /**
    * broadcast envelope to peers
    * @param tracker ballot tracker
    * @param quorumSet quorum set
    * @return
    */
  def broadcastEnvelope(tracker: BallotTracker,
                        quorumSet: QuorumSet,
                        envelope: BallotEnvelope[BallotMessage]): P[F, Delta[BallotTracker]]

  /**
    * when agreement reached, an event triggered
    * @param slotIndex slot index
    * @param value value
    * @return
    */
  def externalizedValue(nodeID: NodeID, slotIndex: SlotIndex, value: Value): P[F, Unit]
}
