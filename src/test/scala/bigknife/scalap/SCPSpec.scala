package bigknife.scalap

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component.Model
import org.scalatest._
import interpreter.{Setting, runner}
import bigknife.sop._
import bigknife.sop.implicits._

class SCPSpec extends FunSuite with GivenWhenThen {
  val fixture: TestFixture = new TestFixture {}
  import fixture._

  test("scp nominate value") {
    info("this is v0 node")
    Given("a simple 4 nodes cluster {{v0, v1, v2, v3}}")
    val localNodeID = simple4nodes.getNode(0) // v0
    val slotIndex   = SlotIndex(1)

    lazy val setting: Setting = newSetting(
      localNodeID,
      simple4nodes.getQuorumSet(0),
      Map(Seq(1, 2, 3).map(x => simple4nodes.getNode(x) -> simple4nodes.getQuorumSet(x)): _*),
      new TestConnect
    )
    val value = TestValue("value 1")
    // 1 - 2 - 3, nominate
    val env1 = simple4nodes.createNominationEnvelope(1, slotIndex, Some(value), None)
    val env2 = simple4nodes.createNominationEnvelope(2, slotIndex, Some(value), None)
    val env3 = simple4nodes.createNominationEnvelope(3, slotIndex, Some(value), None)

    val acceptEnv1 = simple4nodes.createNominationEnvelope(1, slotIndex, Some(value), Some(value))
    val acceptEnv2 = simple4nodes.createNominationEnvelope(2, slotIndex, Some(value), Some(value))
    val acceptEnv3 = simple4nodes.createNominationEnvelope(3, slotIndex, Some(value), Some(value))

    When("run nominate values program")
    val program = for {
      _               <- scp.cacheQuorumSet(simple4nodes.getQuorumSet(1))
      _               <- scp.cacheQuorumSet(simple4nodes.getQuorumSet(2))
      _               <- scp.cacheQuorumSet(simple4nodes.getQuorumSet(3))
      _               <- scp.nominate(localNodeID, slotIndex, round = 1, value, Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 2, value, Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 3, value, Value.bottom)
      _               <- info("nominate value 1").pureSP[Model.Op]
      _               <- scp.processEnvelope(localNodeID, env1)
      _               <- scp.processEnvelope(localNodeID, env2)
      _               <- scp.processEnvelope(localNodeID, env3)
      _               <- scp.processEnvelope(localNodeID, acceptEnv1)
      _               <- scp.processEnvelope(localNodeID, acceptEnv2)
      _               <- scp.processEnvelope(localNodeID, acceptEnv3)
      nominateTracker <- scp.getNominateTracker(localNodeID, slotIndex)
      ballotTracker   <- scp.getBallotTracker(localNodeID, slotIndex)
    } yield (nominateTracker, ballotTracker)
    val (nominateTracker, ballotTracker) = runner.runIO(program, setting).unsafeRunSync()
    info("[v0-nominate-tracker] voted: " + nominateTracker.nomination.voted.toString)
    info("[v0-nominate-tracker] accepted: " + nominateTracker.nomination.accepted.toString)
    info("[v0-nominate-tracker] candidate: " + nominateTracker.candidates)
    info("[v0-ballot-tracker] current" + ballotTracker.current)
  }
}
