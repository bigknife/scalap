package bigknife.scalap
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter.Setting
import bigknife.scalap.interpreter.service.QuorumSetServiceHandler
import org.scalatest._

class ScpFeature extends FeatureSpec with GivenWhenThen {

  feature("nominate value") {
    val env = Fixture
    import env._

    scenario("nominate in a simple quorum function") {
      implicit val combiner = Value.ValueCombiner.summon {values =>
        values.map(_.asInstanceOf[TestValue]).foldLeft(TestValue("")) {(acc, n)=>
          TestValue(acc.str + n.str)
        }
      }

      Given("four nodes, with ID: v1, v2, v3, v4")
      val v1 = env.createNodeId("v1")
      val v2 = env.createNodeId("v2")
      val v3 = env.createNodeId("v3")
      val v4 = env.createNodeId("v4")

      Given(
        "quorum sets with function for every node: Q(v1) = {2{v1, v2, v3}}, Q(v2)=Q(v3)=Q(v4)={2{v2,v3,v4}}")
      val qsV1: QuorumSet = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v1, v2, v3))
      val qsV234          = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v2, v3, v4))

      Given("a slot(0) for node v1")
      val slot = env.initialSlot(v1)

      Given("nominate slot 0 with value 10")
      val value = env.simpleValue(10)

      Given("system setting with node id v1, qsV1")

      def reNominateFunc(setting: => Setting,
                         dummy: Boolean = true): (Slot, ReNominateArgs) => Unit =
        (slot, reNominateArgs) => {
          if (!dummy) {
            val p = for {
              state <- env.scpTest.nominate(slot,
                                            reNominateArgs.value,
                                            reNominateArgs.previousValue,
                                            timeout = reNominateArgs.timeout)
              slot <- env.scpTest.getSlot(slot.nodeId, 0)
            } yield (state, slot)

            val messageState = runner.runIO(p, setting).unsafeRunSync()
            info(s"messageState1 = $messageState")
          } else {
            info("dummy re-nominating")
          }
        }

      lazy val setting: Setting =
        Setting(nodeId = v1, quorumSet = qsV1, reNominate = reNominateFunc(setting))

      Given("create nominate program")
      val p = for {
        state <- env.scpTest.nominate(slot, value, Value.Bottom, timeout = false)
        slot  <- env.scpTest.getSlot(slot.nodeId, 0)
      } yield (state, slot)

      When("run the program")
      val messageState = runner.runIO(p, setting).unsafeRunSync()

      Then("get the message state and slot states")
      info(s"messageState = $messageState")

      Given("nomination message from v2/3/4")
      val nominationMessageFromV2 = Message(
        Message.Nominate(
          nodeId = v2,
          slotIndex = slot.index,
          votes = Vector(env.simpleValue(10)),
          accepted = Vector.empty,
          quorumSetHash = env.hashOfQuorumSet(qsV234, setting)
        ),
        Signature.Empty
      )
      val nominationMessageFromV3 = Message(
        Message.Nominate(
          nodeId = v3,
          slotIndex = slot.index,
          votes = Vector(env.simpleValue(10)),
          accepted = Vector.empty,
          quorumSetHash = env.hashOfQuorumSet(qsV234, setting)
        ),
        Signature.Empty
      )
      val nominationMessageFromV4 = Message(
        Message.Nominate(
          nodeId = v4,
          slotIndex = slot.index,
          votes = Vector(env.simpleValue(10)),
          accepted = Vector.empty,
          quorumSetHash = env.hashOfQuorumSet(qsV234, setting)
        ),
        Signature.Empty
      )

      Then("Before sending message from v2, we should cache the qs of v2/3/4 for v1")
      val pSaveV234QuorumSet = env.scpTest.saveQuorumSet(qsV234)
      runner.runIO(pSaveV234QuorumSet, setting).unsafeRunSync()

      Then("ready for handling message from v2/3/4")
      val pSendFromV2 = env.scpTest.handleMessage(v1, nominationMessageFromV2)
      val pSendFromV3 = env.scpTest.handleMessage(v1, nominationMessageFromV3)
      val pSendFromV4 = env.scpTest.handleMessage(v1, nominationMessageFromV4)

      Then("wait for 2 seconds, received nominate v(10) message from v2/3/4")
      //Thread.sleep(2000)
      //info("\n=======================After 2secs===========================")
      val v2MsgState = runner.runIO(pSendFromV2, setting).unsafeRunSync()
      info(s"message state from v2: $v2MsgState")
      assert(v2MsgState == Message.State.Valid)

      val v3MsgState = runner.runIO(pSendFromV3, setting).unsafeRunSync()
      info(s"message state from v3: $v3MsgState")
      assert(v3MsgState == Message.State.Valid)

      val v4MsgState = runner.runIO(pSendFromV4, setting).unsafeRunSync()
      info(s"message state from v4: $v4MsgState\n")
      assert(v4MsgState == Message.State.Valid)

      Then("get the latest slot(0), we should assert that a new accept value v(10) reached")
      val pGetSlot = env.scpTest.getSlot(v1, 0L)
      val slotOpt  = runner.runIO(pGetSlot, setting).unsafeRunSync()
      assert(slotOpt.nonEmpty)
      val latestSlot: Slot = slotOpt.get
      val latestAccepted   = latestSlot.nominateTracker.accepted
      info(s"slot's accepted: $latestAccepted")
      assert(latestAccepted.head == value)
    }

    scenario("nominate multi values for a slot") {
      implicit val cvb: Value.ValueCombiner = combinableValueCombiner

      Given("for nodes(v1,v2,v3,v4)")
      val v1 = createNodeId("v1")
      val v2 = createNodeId("v2")
      val v3 = createNodeId("v3")
      val v4 = createNodeId("v4")

      Given("with quorum sets: Q(v1)={{v1,v2,v3}},Q(v2,v3,v4)={{v2,v3,v4}}")
      val qs1: QuorumSet = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v1, v2, v3))
      val qs234          = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v2, v3, v4))

      Given("a setting to run")
      val setting = Setting(v1, qs1)

      Given("a slot with index 0 in v1")
      val slotIndex: Long = 0
      val slot = env.initialSlot(v1, slotIndex)

      Given("first value to nominate, cv1")
      val cv1 = combinableValue(1)

      When("nominate to v1 at slot 0 with value cv1")
      runner.runIO(env.scpTest.nominate(slot, cv1, Value.Bottom, timeout = false), setting)
          .unsafeRunSync()

      When("register v2,3,4 quorum set hash to the v1's local store")
      runner.runIO(env.scpTest.saveQuorumSet(qs234), setting)
          .unsafeRunSync()

      Given("nomination message for v2, v3 with value cv1")
      val nm2 = makeNominationMessage(v2, slotIndex, Vector(cv1), Vector.empty, hashOfQuorumSet(qs234, setting))
      val nm3 = makeNominationMessage(v3, slotIndex, Vector(cv1), Vector.empty, hashOfQuorumSet(qs234, setting))

      When("v1 handle nominate message from v2,3 to vote cv1 in slot 0")
      val st1 = runner.runIO(env.scpTest.handleMessage(v1, nm2), setting)
          .unsafeRunSync()
      info(s"handle v2 nomination message: $st1")
      val st2 = runner.runIO(env.scpTest.handleMessage(v1, nm3), setting)
        .unsafeRunSync()
      info(s"handle v3 nomination message: $st2")

      Then("v1 accept CombineValue(1, now()) for slot 0")
      val slot0Opt = runner.runIO(scpTest.getSlot(v1, slotIndex), setting).unsafeRunSync()
      assert(slot0Opt.isDefined)
      info(s"after processing nomination messages from v2,3 now cv1 accepted: ${slot0Opt.get.nominateTracker.accepted}")
      info(s"after processing nomination messages from v2,3 now candidate: ${slot0Opt.get.nominateTracker.candidates}")
      info(s"after processing nomination messages from v2,3 now ballot: ${slot0Opt.get.ballotTracker.commit}")

      Given("the second value for slot 0, cv2")
      val cv2 = combinableValue(2)

      When("nominate to v1 at slot 0 with value cv2, and previous value is cv1")
      val nominateCv2 = runner.runIO(env.scpTest.nominate(slot0Opt.get, cv2, cv1, timeout = false), setting)
        .unsafeRunSync()
      info(s"nominate cv2: $nominateCv2")

      Given("nomination message for v2, v3 with value cv2")
      val nm_2_v2 = makeNominationMessage(v2, slotIndex, Vector(cv1, cv2), Vector(cv1), hashOfQuorumSet(qs234, setting))
      val nm_3_v2 = makeNominationMessage(v3, slotIndex, Vector(cv1, cv2), Vector(cv1), hashOfQuorumSet(qs234, setting))

      When("v1 handle nominate to v1 at slot 0 with value cv2")
      val st_v2_1 = runner.runIO(env.scpTest.handleMessage(v1, nm_2_v2), setting)
        .unsafeRunSync()
      info(s"handle v2 nomination message: $st_v2_1")
      val st_v2_2 = runner.runIO(env.scpTest.handleMessage(v1, nm_3_v2), setting)
        .unsafeRunSync()
      info(s"handle v3 nomination message: $st_v2_2")

      Then("v1 accept cv2 for slot 0")
      val slot1Opt = runner.runIO(scpTest.getSlot(v1, slotIndex), setting).unsafeRunSync()
      assert(slot1Opt.isDefined)
      info(s"after processing nomination messages from v2,3 now cv1 accepted: ${slot1Opt.get.nominateTracker.accepted}")
      assert(slot1Opt.get.nominateTracker.accepted.length == 2)
      info(s"after processing nomination messages from v2,3 now candidate: ${slot1Opt.get.nominateTracker.candidates}")
      info(s"after processing nomination messages from v2,3 now ballot: ${slot1Opt.get.ballotTracker.commit}")

    }
  }
}
