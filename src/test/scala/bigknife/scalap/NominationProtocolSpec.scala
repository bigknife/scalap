package bigknife.scalap

import bigknife.scalap.ast.usecase._
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter._
import bigknife.scalap.interpreter.service.QuorumSetServiceHandler
import cats.kernel.instances.hash
import org.scalatest.FunSuite

class NominationProtocolSpec extends FunSuite {

  def sha3(s: Array[Byte]): Array[Byte] = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    new SHA3.Digest256().digest(s)
  }
  def sha3(s: String): Array[Byte] = sha3(s.getBytes)

  def hashOfQuorumSet(qs: QuorumSet, setting: Setting): Hash = {
    val qsHandler = new QuorumSetServiceHandler
    qsHandler.hashOfQuorumSet(qs)(setting).unsafeRunSync()
  }

  test("test program") {
    // prepare data
    val nodeIds         = Vector("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8").map(x => Node.ID(sha3(x)))
    val slotIndex: Long = 1
    val value = TestValue("hello,world")

    // current node is v1, qs is {{v1,v2,v3}}
    val node1 = nodeIds(0)
    val qs1 = QuorumSet.Empty
      .withThreshold(2)
      .withValidators(nodeIds.take(3))
    val setting1: Setting = Setting(
      nodeId = Node.ID(sha3("v1")),
      qs1
    )

    // node2 {{v2,v3,v4}}
    val node2 = nodeIds(1)
    val qs2 = QuorumSet.Empty
      .withThreshold(2)
      .withValidators(nodeIds.slice(1, 4))

    // node3 {{v2,v3,v4}}
    val node3 = nodeIds(2)
    val qs3 = QuorumSet.Empty
      .withThreshold(2)
      .withValidators(nodeIds.slice(1, 4))

    // node4 {{v2,v3,v4}}
    val node4 = nodeIds(3)
    val qs4 = QuorumSet.Empty
      .withThreshold(2)
      .withValidators(nodeIds.slice(1, 4))

    // first a nomination message from v2
    val nomV2 =
      Message.Nominate(node2, slotIndex, Vector(value), Vector(value), hashOfQuorumSet(qs2, setting1))
    val nomV3 =
      Message.Nominate(node3, slotIndex, Vector(value), Vector(value), hashOfQuorumSet(qs3, setting1))

    val msg1 = Message(nomV2, Signature.Empty)
    val msg2 = Message(nomV3, Signature.Empty)

    val scpTest = SCPTest[component.Model.Op]

    val p1 = scpTest.handleMessage(setting1.nodeId, msg1)
    val p2 = scpTest.handleMessage(setting1.nodeId, msg2)
    val p3 = scpTest.getSlot(setting1.nodeId, slotIndex)

    val p = for {
      _ <- scpTest.saveQuorumSet(qs1)
      _ <- scpTest.saveQuorumSet(qs2)
      _ <- scpTest.saveQuorumSet(qs3)
      _ <- scpTest.saveQuorumSet(qs4)
      _ <- p1
      _ <- p2
      x <- p3
    } yield x.get

    val slot = runner.runIO(p, setting1).unsafeRunSync()
    info(s"slot ballot phrase: ${slot.ballotTracker.phase}")

    info(s"slot: ${slot.nominateTracker.latestCompositeCandidate}")

  }
}
