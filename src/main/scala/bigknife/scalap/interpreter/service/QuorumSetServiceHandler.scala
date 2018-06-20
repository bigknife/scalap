package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.QuorumSetService
import bigknife.scalap.ast.types.Node.ID
import bigknife.scalap.ast.types._
import org.slf4j.{Logger, LoggerFactory}

class QuorumSetServiceHandler extends QuorumSetService.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)


  override def isQuorumSetSane(quorumSet: QuorumSet): Stack[Boolean] = Stack {
    def checkSanity(quorumSet: QuorumSet, depth: Int, extraChecks: Boolean, knownNodes: Vector[Node.ID]): Boolean = {
      val totalSize = quorumSet.validators.length + quorumSet.innerSets.length
      val vBlockingSize = totalSize - quorumSet.threshold + 1

      if (depth > 2 ||
        quorumSet.threshold < 1 ||
        quorumSet.threshold > totalSize ||
        (extraChecks && quorumSet.threshold < vBlockingSize)
      ) false // quorum set is limited to 2 layers, threshold should >= 1
      else {
        // known nodes and presence
        val check = quorumSet.validators.foldLeft((knownNodes, false)) {
          case (x@(nodes, presence), nodeId) => if (presence) {
            if (nodes.contains(nodeId)) (nodes, true) else (nodes :+ nodeId, true)
          } else {
            if (nodes.contains(nodeId)) (nodes, true) else (nodes :+ nodeId, false)
          }
        }
        if (check._2) {
          // todo: check recursively inner
          ???
        } else false
      }
    }

    checkSanity(quorumSet, 0)
  }

  override def isVBlocking(quorumSet: QuorumSet,
                           nodes: Vector[Node.ID]): Stack[Boolean] = Stack {

    def isVBlockingInternal(quorumSet: QuorumSet, nodes: Vector[Node.ID]): Boolean = {
      // there is no v-blocking set for threashold = 0, means, every node can be failed.
      if (quorumSet.threshold <= 0) false
      else {
        val leftTillBlock = (1 + quorumSet.validators.length + quorumSet.innerSets.length) - quorumSet.threshold
        val leftLevel1 = quorumSet.validators.foldLeft(leftTillBlock) {
          (acc, n) =>
            if (nodes.contains(n)) acc - 1
            else acc
        }
        if (leftLevel1 <= 0) true
        else {
          val leftLevel2 = quorumSet.innerSets.foldLeft(leftLevel1) {
            (acc, n) =>
              if (isVBlockingInternal(n, nodes)) acc - 1
              else acc
          }
          leftLevel2 <= 0
        }
      }
    }

    isVBlockingInternal(quorumSet, nodes)
  }

  override def isQuorumSlice(quorumSet: QuorumSet, nodes: Vector[Node.ID]): Stack[Boolean] = Stack {
    def isQuorumSliceInternal(quorumSet: QuorumSet, nodes: Vector[Node.ID]): Boolean = {
      val threasoldLeft = quorumSet.threshold
      val leftLevel1 = quorumSet.validators.foldLeft(threasoldLeft){(acc, n) =>
        if (nodes.contains(n)) acc - 1
        else acc
      }
      if (leftLevel1 <= 0) true
      else {
        val leftLevel2 = quorumSet.innerSets.foldLeft(leftLevel1) {(acc, n) =>
          if (isQuorumSliceInternal(n, nodes)) acc - 1
          else acc
        }

        leftLevel2 <= 0
      }
    }
    isQuorumSliceInternal(quorumSet, nodes)
  }

  override def quorumFunction(nodeId: ID): Stack[QuorumSet] = Stack {setting =>
    logger.debug(s"Q($nodeId)")
    if (setting.nodeId == nodeId) setting.quorumSet
    else QuorumSet.Empty
  }

  override def hashOfQuorumSet(quorumSet: QuorumSet): Stack[Hash] = Stack {
    def _inner(qs: QuorumSet): Vector[Byte] = {
      val bb = java.nio.ByteBuffer.allocate(4)
      bb.putInt(qs.threshold)
      val h1 = bb.array().toVector ++ qs.validators.flatMap(_.value.toVector)
      val h2 = qs.innerSets.flatMap(x => _inner(x))
      h1 ++ h2
    }
    Hash(misc.crypto.sha3(_inner(quorumSet).toArray))
  }
}

object QuorumSetServiceHandler {
  trait Implicits {
    implicit val quorumSetServiceHandler: QuorumSetServiceHandler =
      new QuorumSetServiceHandler
  }
}
