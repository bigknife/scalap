package bigknife.scalap.interpreter.service

import bigknife.scalap.ast.types._
import bigknife.scalap.interpreter.Setting
import bigknife.scalap.util._
import org.scalatest._

class NominateServiceHandlerSpec extends FunSuite with GivenWhenThen {
  trait Fixture {
    val nominateServiceHandler: NominateServiceHandler =
      NominateServiceHandler.implicits.nominateServiceHandler

    val setting: Setting = Setting.default()
  }
  test("findRoundLeaders") {
    val fixture = new Fixture {}
    import fixture._

    Given("nodeIds: v0, v1, v2, v3, v4, v5")
    val nodeIds =
      Vector("v0", "v1", "v2", "v3", "v4", "v5").map(x => NodeID(crypoto.sha3(x.getBytes)))
    info(s"    $nodeIds")

    Given("a quorum set with v0, v1, v2, v3 nest with v4, v5")
    val qSet = QuorumSet
      .simple(3, nodeIds.take(4): _*)
      .nest(1, nodeIds(4), nodeIds(5))

    for (i <- 1 to 50) {
      Given(s"round = $i, slotIndex = 1, previousValue = Value(hello,world)")
      val round: Int = i
      val slotIndex: SlotIndex = SlotIndex(1)
      val previousValue: Value = Value.simple("hello,world".getBytes())

      When("find round leaders")
      val leaders = nominateServiceHandler
        .findRoundLeaders(qSet, round, slotIndex, previousValue)(setting)
        .unsafeRunSync()

      Then("leaders found")
      info(s"    leaders is $leaders")
    }
  }
}
