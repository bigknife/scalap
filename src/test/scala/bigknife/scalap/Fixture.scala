package bigknife.scalap

import bigknife.scalap.ast.service.ApplicationExtension
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.component.Model
import bigknife.scalap.interpreter._
import bigknife.scalap.interpreter.service.QuorumSetServiceHandler

trait Fixture {
  def runner(implicit C: Value.ValueCombiner): Runner = new Runner {
    override implicit val applicationExtension: ApplicationExtension.Handler[Stack] =
      new TestApplicationExtension {
        override implicit val valueCombiner: Value.ValueCombiner = C
      }
  }
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
    override def toString: String  = s"Value($value)"
    override def orderFactor: Long = value.toLong
    override def asBytes: Array[Byte] = {
      import java.nio._
      val bb = ByteBuffer.allocate(4)
      bb.putInt(value)
      bb.array()
    }
  }
  def simpleValue(i: Int): Value = SimpleValue(i)
  val simpleValueCombiner: Value.ValueCombiner = Value.ValueCombiner.summon {values =>
    values.head
  }

  def initialSlot(nodeId: Node.ID, slotIndex: Long = 0): Slot = {
    Slot(nodeId, slotIndex, NominateTracker.Empty, BallotTracker.Empty, Vector.empty, fullValidated = true)
  }

  private case class CombinableValue(values: Vector[Int], timestamp: Long) extends Value {
    override def orderFactor: Long = timestamp
    override def asBytes: Array[Byte] = {
      values.map { value =>
        import java.nio._
        val bb = ByteBuffer.allocate(4)
        bb.putInt(value)
        bb.array()
      }
    }.fold(Array.emptyByteArray)(_ ++ _)
  }
  def combinableValue(i: Int): Value = CombinableValue(Vector(i), System.currentTimeMillis())
  val combinableValueCombiner: Value.ValueCombiner = Value.ValueCombiner.summon {values =>
    values.map(_.asInstanceOf[CombinableValue]).foldLeft(CombinableValue(Vector.empty, 0)){(acc, n) =>
      CombinableValue(acc.values ++ n.values, n.timestamp)
    }
  }


  def makeNominationMessage(nodeId: Node.ID,
                            slotIndex: Long,
                            votes: Vector[Value],
                            accepted: Vector[Value],
                            qsHash: Hash): NominationMessage = {
    Message(Message.Nominate(
              nodeId,
              slotIndex,
              votes,
              accepted,
              qsHash
            ),
            Signature.Empty)
  }
}

object Fixture extends Fixture
