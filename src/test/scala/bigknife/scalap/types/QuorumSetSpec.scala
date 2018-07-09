package bigknife.scalap.types

import bigknife.scalap.ast.types._
import org.scalatest._
import bigknife.scalap.util._

class QuorumSetSpec extends FeatureSpec with GivenWhenThen {
  def isNear(r: Long, target: Double): Boolean = {
    val v = r / Long.MaxValue.toDouble
    (v - target).abs < 0.01
  }

  feature("quorum set can measure a node's weight") {
    scenario("node not in quorum set, then the weight is 0") {
      Given("nodeIds: v0, v1, v2, v3, v4, v5")
      val nodeIds =
        Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
      info(s"    $nodeIds")

      Given("a quorum set with v0, v1, v2, v3")
      val qSet = QuorumSet.simple(3, nodeIds.take(4): _*)

      When("measuring a node v4")
      val result = qSet.nodeWeight(nodeIds(4))

      Then("the weight is 0")
      info(s"    weight = $result")
      assert(result == 0)
    }
    scenario("node in a simple quorum set, the weight is near threshold / total size") {
      Given("nodeIds: v0, v1, v2, v3, v4, v5")
      val nodeIds =
        Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
      info(s"    $nodeIds")

      Given("a quorum set with v0, v1, v2, v3")
      var qSet = QuorumSet.simple(3, nodeIds.take(4): _*)

      When("measuring a node v2")
      var result = qSet.nodeWeight(nodeIds(2))


      Then("the weight is 0.75")
      info(s"    weight = $result")
      assert(isNear(result, target = 0.75))

      Given("a quorum set with v0, v1, v2")
      qSet = QuorumSet.simple(2, nodeIds.take(3): _*)

      When("measuring a node v0")
      result = qSet.nodeWeight(nodeIds(0))

      Then("the weight is 0.66")
      assert(isNear(result, target = 0.66))
    }

    scenario("node in a nest quorum set, the weight is ???") {
      Given("nodeIds: v0, v1, v2, v3, v4, v5")
      val nodeIds =
        Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
      info(s"    $nodeIds")

      Given("a quorum set with v0, v1, v2, v3")
      var qSet = QuorumSet.simple(3, nodeIds.take(4): _*)
        .nest(1, nodeIds(4), nodeIds(5))

      When("measuring a node v4")
      var result = qSet.nodeWeight(nodeIds(4))
      info(s"    weight = $result")

      Then("the weight is 0.03")
      assert(isNear(result, 0.6 * 0.5))
    }

    scenario("a nest quorum set nodes") {
      Given("nodeIds: v0, v1, v2, v3, v4, v5")
      val nodeIds =
        Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
      info(s"    $nodeIds")

      Given("a quorum set with v0, v1, v2, v3 nest with v4, v5")
      var qSet = QuorumSet.simple(3, nodeIds.take(4): _*)
        .nest(1, nodeIds(4), nodeIds(5))

      When("calculate the hold nodeIds")
      val whole = qSet.allNodes

      Then("got 6 nodeIds")
      info(s"    $whole")
      assert(whole.size == 6)

    }

    scenario("compute neighbors") {
      Given("nodeIds: v0, v1, v2, v3, v4, v5")
      val nodeIds =
        Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
      info(s"    $nodeIds")

      Given("a quorum set with v0, v1, v2, v3 nest with v4, v5")
      var qSet = QuorumSet.simple(3, nodeIds.take(4): _*)
        .nest(1, nodeIds(4), nodeIds(5))

      Given("round = 1, slotIndex = 1, previousValue = Value(hello,world)")
      val round: Int = 1
      val slotIndex: SlotIndex = SlotIndex(1)
      val previousValue: Value = Value("hello,world".getBytes())

      When("calculate neighbors")
      val n = qSet.neighbors(round, slotIndex, previousValue)
      info(s"    $n")
    }
  }
}
