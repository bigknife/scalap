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
  def recordEnvelope[M <: BallotMessage](tracker: BallotTracker, envelope: BallotEnvelope[M]): P[F, BallotTracker]

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
    * try to set prepare ballot to a tracker
    * @param tracker tracker
    * @param ballot ballot
    * @return
    */
  def setPrepared(tracker: BallotTracker, ballot: Ballot): P[F, Delta[BallotTracker]]

  /**
    * check if we also need to clear commit of tracker
    * @param tracker tracker
    * @return
    */
  def clearCommitIfNeeded(tracker: BallotTracker): P[F, Delta[BallotTracker]]
}
