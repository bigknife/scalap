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

  private def hashNode(slot: Slot, isPriority: Boolean, nodeId: Node.ID): Long = {
    import java.nio._
    val f1 = {
      val bb = ByteBuffer.allocate(8)
      val n = slot.index
      bb.putLong(n)
      bb.array()
    }
    val f2 = {
      val bb = ByteBuffer.allocate(4)
      val n = slot.nominateTracker.roundNumber
      bb.putInt(n)
      bb.array()
    }
    val f3 = {
      val n = if(isPriority) 2 else 1
      val bb = ByteBuffer.allocate(4)
      bb.putInt(n)
      bb.array()
    }
    val f4 = nodeId.value
    val f5 = slot.nominateTracker.previousValue.get.asBytes
    val bytes = misc.crypto.sha3(f1 ++ f2 ++ f3++ f4 ++ f5)
    val l = new java.math.BigInteger(bytes).longValue()
    //asUnsignedLong(l)
    l
  }

  private def bigDivide(a: Long, b: Long, c: Long): Long = {
    val a1 = BigDecimal(a)
    val b1 = BigDecimal(b)
    val c1 = BigDecimal(c)

    val l = ((a1 * b1) / c1).setScale(0, RoundingMode.UP).longValue()
    //asUnsignedLong(l)
    l
  }

  private def asUnsignedLong(l: Long): Long = {
    val bi = java.math.BigInteger.valueOf(l)
    if (bi.signum() < 0) bi.add(java.math.BigInteger.ONE.shiftLeft(64)).toString().toLong else l
    //l.abs
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
