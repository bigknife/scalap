package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.QuorumSetService
import bigknife.scalap.ast.types.Node.ID
import bigknife.scalap.ast.types._
import org.slf4j.{Logger, LoggerFactory}

class QuorumSetServiceHandler extends QuorumSetService.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def isQuorumSetSane(quorumSet: QuorumSet, extractChecks: Boolean): Stack[Boolean] =
    Stack { setting =>
      def checkSanity(quorumSet: QuorumSet,
                      depth: Int,
                      extraChecks: Boolean,
                      knownNodes: Vector[Node.ID],
                      nodesCount: Int): (Boolean, Int) = {
        val totalSize     = quorumSet.validators.length + quorumSet.innerSets.length
        val vBlockingSize = totalSize - quorumSet.threshold + 1

        if (depth > 2 ||
            quorumSet.threshold < 1 ||
            quorumSet.threshold > totalSize ||
            (extraChecks && quorumSet.threshold < vBlockingSize))
          (false, nodesCount + quorumSet.validators.size) // quorum set is limited to 2 layers, threshold should >= 1
        else {
          // known nodes and presence
          val check = quorumSet.validators.foldLeft((knownNodes, false)) {
            case (x @ (nodes, presence), nodeId) =>
              if (presence) {
                if (nodes.contains(nodeId)) (nodes, true) else (nodes :+ nodeId, true)
              } else {
                if (nodes.contains(nodeId)) (nodes, true) else (nodes :+ nodeId, false)
              }
          }
          // if not present, go on the inners
          if (!check._2) {
            val innerSetResult = quorumSet.innerSets.foldLeft((nodesCount + quorumSet.validators.length, true)) {
              case ((c, sane), qs) =>
                if (sane) {
                  val res = checkSanity(qs,
                                        depth + 1,
                                        extraChecks,
                                        check._1,
                                        nodesCount + qs.validators.size)
                  (res._2, res._1)
                } else (c, sane)
            }
            (innerSetResult._2, innerSetResult._1)
          } else (false, nodesCount + quorumSet.validators.size)
        }
      }

      val checkResult = checkSanity(quorumSet, 0, extractChecks, Vector.empty, 0)
      checkResult._1 && (checkResult._2 >= 1 && checkResult._2 <= setting.maxNodesInQuorumSet)
    }

  override def isVBlocking(quorumSet: QuorumSet, nodes: Vector[Node.ID]): Stack[Boolean] = Stack {

    def isVBlockingInternal(quorumSet: QuorumSet, nodes: Vector[Node.ID]): Boolean = {
      // there is no v-blocking set for threashold = 0, means, every node can be failed.
      if (quorumSet.threshold <= 0) false
      else {
        val leftTillBlock = (1 + quorumSet.validators.length + quorumSet.innerSets.length) - quorumSet.threshold
        val leftLevel1 = quorumSet.validators.foldLeft(leftTillBlock) { (acc, n) =>
          if (nodes.contains(n)) acc - 1
          else acc
        }
        if (leftLevel1 <= 0) true
        else {
          val leftLevel2 = quorumSet.innerSets.foldLeft(leftLevel1) { (acc, n) =>
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
      val thresholdLeft = quorumSet.threshold
      val leftLevel1 = quorumSet.validators.foldLeft(thresholdLeft) { (acc, n) =>
        if (nodes.contains(n)) acc - 1
        else acc
      }
      if (leftLevel1 <= 0) true
      else {
        val leftLevel2 = quorumSet.innerSets.foldLeft(leftLevel1) { (acc, n) =>
          if (isQuorumSliceInternal(n, nodes)) acc - 1
          else acc
        }

        leftLevel2 <= 0
      }
    }
    isQuorumSliceInternal(quorumSet, nodes)
  }

  override def quorumFunction(nodeId: ID): Stack[QuorumSet] = Stack { setting =>
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

  override def buildSingletonQuorumSet(nodeId: ID): Stack[QuorumSet] = Stack {
    QuorumSet(
      threshold = 1,
      validators = Vector(nodeId)
    )
  }

  override def normalizeQuorumSet(quorumSet: QuorumSet, toRemove: ID): Stack[QuorumSet] = Stack {
    def normalizeQSet(qs: QuorumSet, toRemove: Node.ID): QuorumSet = {
      // helper function that:
      //  * removes nodeID
      //      { t: n, v: { ...BEFORE... , nodeID, ...AFTER... }, ...}
      //      { t: n-1, v: { ...BEFORE..., ...AFTER...} , ... }
      //  * simplifies singleton inner set into outerset
      //      { t: n, v: { ... }, { t: 1, X }, ... }
      //        into
      //      { t: n, v: { ..., X }, .... }
      //  * simplifies singleton innersets
      //      { t:1, { innerSet } } into innerSet
      val validatorsRemoved = qs.validators.dropWhile(_ == toRemove)
      val qs1 = qs.copy(validators = validatorsRemoved)
      val qs2 = qs1.innerSets.foldLeft(qs1) {(acc, n) =>
        val qsx = normalizeQSet(n, toRemove)
        // merge singleton inner sets into validator list
        if (qsx.threshold == 1 && qsx.validators.length == 1 && qsx.innerSets.length == 0) {
          acc.copy(
            validators = acc.validators :+ qsx.validators.head,
            innerSets = acc.innerSets.dropWhile(_ == n)
          )
        }else acc
      }
      // simplify quorum set if needed
      if (qs2.threshold == 1 && qs2.validators.isEmpty && qs2.innerSets.length == 1) {
        qs2.innerSets.head
      } else qs2
    }

    normalizeQSet(quorumSet, toRemove)
  }
}

object QuorumSetServiceHandler {
  trait Implicits {
    implicit val quorumSetServiceHandler: QuorumSetServiceHandler =
      new QuorumSetServiceHandler
  }
}
