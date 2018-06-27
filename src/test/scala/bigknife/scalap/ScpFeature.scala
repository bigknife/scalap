package bigknife.scalap
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter.Setting
import bigknife.scalap.interpreter.service.QuorumSetServiceHandler
import org.scalatest._

class ScpFeature extends FeatureSpec with GivenWhenThen {
  trait Environment {
    val scpTest: SCPTest[Model.Op] = SCPTest[Model.Op]

    def sha3(s: Array[Byte]): Array[Byte] = {
      import org.bouncycastle.jcajce.provider.digest.SHA3
      new SHA3.Digest256().digest(s)
    }
    def sha3(s: String): Array[Byte] = sha3(s.getBytes)

    def hashOfQuorumSet(qs: QuorumSet, setting: Setting): Hash = {
      val qsHandler = new QuorumSetServiceHandler
      qsHandler.hashOfQuorumSet(qs)(setting).unsafeRunSync()
    }

    def createNodeId(seed: String): Node.ID = Node.ID(sha3(seed))

    private case class SimpleValue(value: Int) extends Value {
      override def toString: String = s"Value($value)"
      override def orderFactor: Int = value
      override def asBytes: Array[Byte] = {
        import java.nio._
        val bb = ByteBuffer.allocate(4)
        bb.putInt(value)
        bb.array()
      }
    }
    def simpleValue(i: Int): Value = SimpleValue(i)

    def initialSlot(nodeId: Node.ID): Slot = {
      Slot(nodeId,
           0,
           NominateTracker.Empty,
           BallotTracker.Empty,
           Vector.empty,
           fullValidated = true)
    }
    val t: ExecutorService = Executors.newFixedThreadPool(1)

    def submitTask(f: () => Unit): Unit = {
      t.submit(new Runnable {
        override def run(): Unit = f()
      })
      ()
    }

    def shutdown(): Unit = {
      t.shutdown()
      ()
    }
    def awaitTaskShutdown(): Unit = {
      t.awaitTermination(Long.MaxValue, TimeUnit.SECONDS)
      ()
    }
  }

  feature("nominate value") {
    val env = new Environment {}

    scenario("nominate in a simple quorum function") {
      val mainTask: () => Unit = { () =>
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
              val reNominateTask: () => Unit = { () =>
                val p = for {
                  state <- env.scpTest.nominate(slot,
                                                reNominateArgs.value,
                                                reNominateArgs.previousValue,
                                                timeout = reNominateArgs.timeout)
                  slot <- env.scpTest.getSlot(slot.nodeId, 0)
                } yield (state, slot)

                val messageState = runner.runIO(p, setting).unsafeRunSync()
                info(s"messageState1 = $messageState")
              }
              env.submitTask(reNominateTask)
            } else {
              info("dummy re-nominating")
            }
          }

        lazy val setting: Setting =
          Setting(nodeId = v1,
                  quorumSet = qsV1,
                  reNominate = reNominateFunc(setting, dummy = false))

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

        Then("wait for 2 seconds, received nominate v(10) message from v2")
        Thread.sleep(2000)
        info("\n=======================After 2secs===========================")
        val v2MsgState = runner.runIO(pSendFromV2, setting).unsafeRunSync()
        info(s"message state from v2: $v2MsgState\n")
        assert(v2MsgState == Message.State.Valid)

        Then("wait for 2 seconds, received nominate v(10) message from v3")
        Thread.sleep(2000)
        info("\n=======================After 2secs===========================")
        val v3MsgState = runner.runIO(pSendFromV3, setting).unsafeRunSync()
        info(s"message state from v3: $v3MsgState\n")
        assert(v3MsgState == Message.State.Valid)

        Then("get the latest slot(0), we should assert that a new accept value v(10) reached")
        val pGetSlot = env.scpTest.getSlot(v1, 0L)
        val slotOpt  = runner.runIO(pGetSlot, setting).unsafeRunSync()
        assert(slotOpt.nonEmpty)
        val latestSlot: Slot = slotOpt.get
        val latestAccepted   = latestSlot.nominateTracker.accepted
        info(s"slot's accepted: $latestAccepted")
        assert(latestAccepted.head == value)
        env.shutdown()
        ()
      }
      env.submitTask(mainTask)
      env.awaitTaskShutdown()
    }


  }
}
