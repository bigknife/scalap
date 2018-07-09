package bigknife.scalap.ast.store

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.sop.macros._

@sp trait NodeStore[F[_]] {
  def getNominateTracker(nodeID: NodeID, slotIndex: SlotIndex): P[F, NominateTracker]
  def getBallotTracker(nodeID: NodeID, slotIndex: SlotIndex): P[F, BallotTracker]

  def getQuorumSet(nodeID: NodeID): P[F, QuorumSet]

  /**
    * get quorum set from a statement
    * @param statement statement
    * @return
    */
  def getQuorumSetFromStatement[M <: Message](statement: Statement[M]): P[F, QuorumSet]

  def cacheQuorumSet(quorumSet: QuorumSet): P[F, Unit]

  def saveNominateTracker(nodeID: NodeID, nominateTracker: NominateTracker): P[F, Unit]
  def saveBallotTracker(nodeID: NodeID, ballotTracker: BallotTracker): P[F, Unit]

  def saveHistoricalStatement[M <: Message](statement: Statement[M]): P[F,Unit]
}
