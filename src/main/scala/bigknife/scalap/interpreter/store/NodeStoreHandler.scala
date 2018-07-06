package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.NodeStore
import bigknife.scalap.ast.types._
import scala.collection._

class NodeStoreHandler extends NodeStore.Handler[Stack] {
  type TrackerKey          = (NodeID, SlotIndex)
  type HistoricalStatement = (Statement[Message], Long)

  private val nominateTrackerStore: mutable.Map[TrackerKey, NominateTracker] = mutable.Map.empty
  private val ballotTrackerStore: mutable.Map[TrackerKey, BallotTracker]     = mutable.Map.empty

  private val historicalStatementStore: mutable.ListBuffer[HistoricalStatement] =
    mutable.ListBuffer.empty
  private val quorumSetStore: mutable.Map[Hash, QuorumSet] = mutable.Map.empty

  override def getNominateTracker(nodeID: NodeID, slotIndex: SlotIndex): Stack[NominateTracker] =
    Stack {
      val key = (nodeID, slotIndex)
      if (nominateTrackerStore.contains(key)) {
        nominateTrackerStore(key)
      } else {
        val nominateTracker = NominateTracker.newTracker(nodeID, slotIndex)
        nominateTrackerStore.put(key, nominateTracker)
        nominateTracker
      }
    }

  override def getBallotTracker(nodeID: NodeID, slotIndex: SlotIndex): Stack[BallotTracker] =
    Stack {
      val key = (nodeID, slotIndex)
      if (ballotTrackerStore.contains(key)) {
        ballotTrackerStore(key)
      } else {
        val ballotTracker = BallotTracker.newTracker(nodeID, slotIndex)
        ballotTrackerStore.put(key, ballotTracker)
        ballotTracker
      }
    }

  override def getQuorumSet(nodeID: NodeID): Stack[QuorumSet] = Stack { setting =>
    if (setting.localNodeID == nodeID) setting.quorumSet
    else if (setting.presetQuorumSets.contains(nodeID)) setting.presetQuorumSets(nodeID)
    else QuorumSet.fake
  }

  override def getQuorumSetFromStatement[M <: Message](statement: Statement[M]): Stack[QuorumSet] =
    Stack { setting =>
      if (setting.localNodeID == statement.nodeID) setting.quorumSet
      else if (setting.presetQuorumSets.contains(statement.nodeID))
        setting.presetQuorumSets(statement.nodeID)
      else {
        statement.message match {
          case x: Message.Externalize => QuorumSet.singleton(statement.nodeID)
          case _ =>
            val quorumSetHash = statement.quorumSetHash
            quorumSetStore.getOrElse(quorumSetHash, QuorumSet.fake)
        }
      }
    }

  override def cacheQuorumSet(quorumSet: QuorumSet): Stack[Unit] = Stack {
    quorumSetStore.put(quorumSet.hash, quorumSet)
    ()
  }

  override def saveNominateTracker(nodeID: NodeID, nominateTracker: NominateTracker): Stack[Unit] =
    Stack {
      nominateTrackerStore.put((nodeID, nominateTracker.slotIndex), nominateTracker)
      ()
    }

  override def saveBallotTracker(nodeID: NodeID, ballotTracker: BallotTracker): Stack[Unit] =
    Stack {
      ballotTrackerStore.put((nodeID, ballotTracker.slotIndex), ballotTracker)
      ()
    }

  override def saveHistoricalStatement[M <: Message](statement: Statement[M]): Stack[Unit] =
    Stack {
      historicalStatementStore.append((statement, System.currentTimeMillis()))
    }
}

object NodeStoreHandler {
  private val _instance = new NodeStoreHandler
  trait Implicits {
    implicit val nodeStoreHandler: NodeStoreHandler = new NodeStoreHandler
  }
}
