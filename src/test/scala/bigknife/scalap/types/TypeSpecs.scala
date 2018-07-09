package bigknife.scalap
package types

import bigknife.scalap.ast.types._
import org.scalatest.FunSuite

class TypeSpecs extends FunSuite {
  test("NodeID equality") {
    val v1 = util.crypoto.sha3("v2".getBytes())
    val v2 = util.crypoto.sha3("v2".getBytes())

    val nodeId1 = NodeID(v1)
    val nodeId2 = NodeID(v2)
    assert(nodeId1 == nodeId2)
    assert(nodeId1 === nodeId2)

    info(nodeId1.toString)

  }

  test("SlotIndex Ordered") {
    val slotIndex1 = SlotIndex(1)
    val slotIndex11 = SlotIndex(1)
    val slotIndex2 = SlotIndex(2)
    assert(slotIndex1 == slotIndex11)
    assert(slotIndex1 < slotIndex2)
    assert(slotIndex2 > slotIndex1)
    assert(slotIndex2 != slotIndex1)
    assert(!(slotIndex1 != slotIndex11))
  }

  test("QuorumSet spec") {
    val v1 = util.crypoto.sha3("v1".getBytes())
    val v2 = util.crypoto.sha3("v2".getBytes())
    val v3 = util.crypoto.sha3("v3".getBytes())

    val qs1 = QuorumSet.simple(2, NodeID(v1), NodeID(v2), NodeID(v3))
    assert(qs1.threshold == 2)
    assert(qs1.size == 3)
    info(qs1.toString)

    val qs2 = QuorumSet.simple(3, NodeID(v1), NodeID(v2), NodeID(v3))
      .nest(1, NodeID(v1))
      .nest(1, NodeID(v2))
      .nest(1, NodeID(v3))
    assert(qs2.size == 6)
    assert(qs2.threshold == 3)
    info(qs2.toString)

    val qs3 =  QuorumSet.simple(2, NodeID(v1), NodeID(v2), NodeID(v3))
    assert(qs1 == qs3)
  }

  test("LinkedHashSet spec") {
    val lhs = LinkedHashSet.empty[Int]("Test") + 1
    info(lhs.toString)
  }
  test("ValueSet spec") {
    val v1 = util.crypoto.sha3("v1".getBytes())
    val v2 = util.crypoto.sha3("v2".getBytes())
    val v3 = util.crypoto.sha3("v3".getBytes())

    val values = ValueSet(Value(v1), Value(v1), Value(v2), Value(v3), Value(v2))
    info(values.toString)

    val values1 = ValueSet(Value(v1), Value(v2), Value(v3))
    info(values1.toString)

    assert(values == values1)
  }

  test("Message Spec") {
    val v1 = util.crypoto.sha3("v1".getBytes())
    val v2 = util.crypoto.sha3("v2".getBytes())
    val v3 = util.crypoto.sha3("v3".getBytes())

    val msg = Message.nominationBuilder().vote(Value(v1), Value(v2)).accept(Value(v2), Value(v3)).build()
    info(msg.toString)
  }
}
