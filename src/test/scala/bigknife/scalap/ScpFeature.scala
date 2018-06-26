package bigknife.scalap
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter.Setting
import org.scalatest._

class ScpFeature extends FeatureSpec with GivenWhenThen {
  trait Environment {
    val scpTest: SCPTest[Model.Op] = SCPTest[Model.Op]

    def sha3(s: Array[Byte]): Array[Byte] = {
      import org.bouncycastle.jcajce.provider.digest.SHA3
      new SHA3.Digest256().digest(s)
    }
    def sha3(s: String): Array[Byte] = sha3(s.getBytes)

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
      Slot(nodeId, 0,
        NominateTracker.Empty,
        BallotTracker.Empty,
        Vector.empty,
        fullValidated = true)
    }
  }


  feature("nominate value") {
    val env = new Environment {}

    scenario("nominate in a simple quorum function") {
      Given("four nodes, with ID: v1, v2, v3, v4")
      val v1 = env.createNodeId("v1")
      val v2 = env.createNodeId("v2")
      val v3 = env.createNodeId("v3")
      val v4 = env.createNodeId("v4")

      Given("quorum sets with function for every node: Q(v1) = {2{v1, v2, v3}}, Q(v2)=Q(v3)=Q(v4)={2{v2,v3,v4}}")
      val qsV1: QuorumSet = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v1, v2, v3))
      val qsV234 = QuorumSet.Empty.copy(threshold = 2).withValidators(Vector(v2, v3, v4))

      Given("a slot(0) for node v1")
      val slot = env.initialSlot(v1)

      Given("nominate slot 0 with value 10")
      val value = env.simpleValue(10)

      Given("system setting with node id v1, qsV1")
      lazy val setting: Setting = Setting(nodeId = v1, quorumSet = qsV1,
        reNominate = (slot, reNominateArgs) => {
          val p = for {
            state <- env.scpTest.nominate(slot, reNominateArgs.value, reNominateArgs.previousValue, timeout = reNominateArgs.timeout)
            slot <- env.scpTest.getSlot(slot.nodeId, 0)
          } yield (state, slot)

          val messageState = runner.runIO(p, setting).unsafeRunSync()
          info(s"messageState1 = $messageState")

        })

      Given("create nominate program")
      val p = for {
        state <- env.scpTest.nominate(slot, value, Value.Bottom, timeout = false)
        slot <- env.scpTest.getSlot(slot.nodeId, 0)
      } yield (state, slot)

      When("run the program")
      val messageState = runner.runIO(p, setting).unsafeRunSync()

      Then("get the message state and slot states")
      info(s"messageState = $messageState")

      Thread.sleep(1000000)
    }
  }
}
