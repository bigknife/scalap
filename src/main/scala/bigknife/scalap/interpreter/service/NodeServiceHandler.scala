package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.NodeService
import bigknife.scalap.ast.types.Node.ID
import bigknife.scalap.ast.types.{Node, QuorumSet, Slot}

import scala.math.BigDecimal.RoundingMode

class NodeServiceHandler extends NodeService.Handler[Stack] {
  override def getNodePriority(slot: Slot, nodeId: ID, qs: QuorumSet): Stack[Long] = Stack {
    val w = if (nodeId == slot.nodeId) Long.MaxValue else getNodeWeight(nodeId, qs)
    if (hashNode(slot, isPriority = false, nodeId) < w) hashNode(slot, isPriority = true, nodeId)
    else 0
  }

  private def getNodeWeight(nodeId: Node.ID, qs: QuorumSet): Long = {
    val n = qs.threshold.toLong
    val d = (qs.innerSets.length + qs.validators.length).toLong

    qs.validators.find(_ == nodeId).map(_ => bigDivide(Long.MaxValue, n, d))
      .orElse(qs.innerSets.find(qsi => {
        val leafW = getNodeWeight(nodeId, qsi)
        leafW > 0
      }).map(x => {
        val leafW = getNodeWeight(nodeId, x)
        bigDivide(leafW, n, d)
      }))
      .getOrElse(0)
  }

  private def hashNode(slot: Slot, isPriority: Boolean, nodeId: Node.ID): Long = ???

  private def bigDivide(a: Long, b: Long, c: Long): Long = {
    val a1 = BigDecimal(a)
    val b1 = BigDecimal(b)
    val c1 = BigDecimal(c)

    ((a1 * b1) / c1).setScale(0, RoundingMode.UP).longValue()
  }

  private def _bigDivide(a: Long, b: Long, c: Long): (Long, Boolean) = {
    val a1 = BigDecimal(a)
    val b1 = BigDecimal(b)
    val c1 = BigDecimal(c)

    val r = ((a1 * b1) / c1).setScale(0, RoundingMode.UP)
    (r.toLong, r <= BigDecimal(Long.MaxValue))
  }
}

object NodeServiceHandler {
  trait Implicits {
    implicit val nodeServiceHandler: NodeServiceHandler = new NodeServiceHandler
  }
}
