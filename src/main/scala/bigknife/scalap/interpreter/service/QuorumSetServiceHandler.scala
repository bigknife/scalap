package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.QuorumSetService
import bigknife.scalap.ast.types._

class QuorumSetServiceHandler extends QuorumSetService.Handler[Stack] {
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
}

object QuorumSetServiceHandler {
  trait Implicits {
    implicit val quorumSetServiceHandler: QuorumSetServiceHandler =
      new QuorumSetServiceHandler
  }
}
