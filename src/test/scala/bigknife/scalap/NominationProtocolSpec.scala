package bigknife.scalap

import bigknife.scalap.ast.usecase._
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter._
import org.scalatest.FunSuite

class NominationProtocolSpec extends FunSuite {

  def sha3(s: Array[Byte]): Array[Byte] = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    new SHA3.Digest256().digest(s)
  }
  def sha3(s: String): Array[Byte] = sha3(s.getBytes)

  def hashOfQuorumSet(qs: QuorumSet): Hash = {
    def _inner(qs: QuorumSet): Vector[Byte] = {
      val h1 = s"${qs.threshold}".toVector.map(_.toByte) ++ qs.validators.flatMap(_.value.toVector)
      val h2 = qs.innerSets.flatMap(x => _inner(x))
      h1 ++ h2
    }

    Hash(sha3(_inner(qs).toArray))
  }

  test("test program") {

    info(s"scp is $scp")

    val quorumSet = QuorumSet(
      3, Vector("v1", "v2", "v3", "v4").map(x => Node.ID(sha3(x))),
      Vector(
        QuorumSet(3, Vector("v1", "v3", "v5", "v7").map(x => Node.ID(sha3(x))), Vector.empty),
        QuorumSet(3, Vector("v1", "v2", "v4", "v6").map(x => Node.ID(sha3(x))), Vector.empty)
      )
    )

    val hash = hashOfQuorumSet(quorumSet)
    val hashHex = hash.value.map("%02x" format _).mkString("")

    info(s"hashHex = $hashHex")

    val value = new Value {
      override def orderFactor: Int = 1

      override def asBytes: Array[Byte] = "hello,world".getBytes

      override def toString: String = s"this is value of test, order factor: $orderFactor"
    }

    val nomination = Message.Nominate(quorumSet.validators(0), 1, Vector(value), Vector(value), hash)

    val msg = Message(
      nomination,
      Signature(Array.emptyByteArray)
    )

    val scpTest = SCPTest[component.Model.Op]

    val p = scpTest.handleMessage(nomination.nodeId, msg)

    runner.runIO(p, Setting()).unsafeRunSync()

  }
}
