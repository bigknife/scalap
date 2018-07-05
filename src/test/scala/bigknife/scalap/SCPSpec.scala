package bigknife.scalap

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component.Model
import org.scalatest._
import interpreter.runner
import bigknife.sop._
import bigknife.sop.implicits._

class SCPSpec extends FunSuite with GivenWhenThen {
  val fixture: TestFixture = new TestFixture {}
  import fixture._

  test("scp nominate value") {
    info(s"scp is $scp")
    val nodeIDs = (1 to 10).toVector.map(x => "v" + x).map(createNodeId)
    Given("local node, slotIndex, quorumset and setting")
    val localNodeID = nodeIDs(0) // v1
    val quorumSet   = QuorumSet.simple(3, nodeIDs.take(4): _*)
    val slotIndex   = SlotIndex(1)
    val setting     = newSetting(localNodeID, quorumSet)

    Given("envelope from v2, v3")
    val qs2 = QuorumSet.simple(3, nodeIDs.take(4): _*)
    val qs3 = QuorumSet.simple(3, nodeIDs.take(4): _*)
    val nom1 = Statement.Nominate(nodeIDs(1),
                                  slotIndex,
                                  qs2.hash,
                                  Message.nominationBuilder().vote(TestValue("value 1")).build())
    val nom2 = Statement.Nominate(nodeIDs(1),
                                  slotIndex,
                                  qs3.hash,
                                  Message.nominationBuilder().vote(TestValue("value 1")).build())

    val nom3 = Statement.Nominate(nodeIDs(1),
      slotIndex,
      qs3.hash,
      Message.nominationBuilder().vote(TestValue("value 1")).build())

    val env1 = Envelope.NominationEnvelope(nom1, Signature.empty)
    val env2 = Envelope.NominationEnvelope(nom2, Signature.empty)
    val env3 = Envelope.NominationEnvelope(nom3, Signature.empty)

    When("run nominate values program")
    val program = for {
      _               <- scp.nominate(localNodeID, slotIndex, round = 1, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 2, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 3, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 4, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 5, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 6, TestValue("value 1"), Value.bottom)
      _               <- scp.nominate(localNodeID, slotIndex, round = 7, TestValue("value 1"), Value.bottom)
      _               <- info("nominate value 1").pureSP[Model.Op]
      nominateTracker <- scp.getNominateTracker(localNodeID, slotIndex)
      ballotTracker   <- scp.getBallotTracker(localNodeID, slotIndex)
      _               <- scp.processNominationEnvelope(localNodeID, env1)
      _               <- scp.processNominationEnvelope(localNodeID, env2)
      _               <- scp.processNominationEnvelope(localNodeID, env3)
    } yield (nominateTracker, ballotTracker)
    val (nominateTracker, ballotTracker) = runner.runIO(program, setting).unsafeRunSync()
    info("voted: " + nominateTracker.nomination.voted.toString)
    info("accepted: " + nominateTracker.nomination.accepted.toString)
  }
}
