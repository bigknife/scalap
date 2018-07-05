package bigknife.scalap

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.SCP
import bigknife.scalap.ast.usecase.component._
import bigknife.scalap.interpreter.Setting
import bigknife.scalap.world.Connect

trait TestFixture {
  val scp: SCP[Model.Op] = SCP[Model.Op]

  def sha3(s: Array[Byte]): Array[Byte] = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    new SHA3.Digest256().digest(s)
  }
  def sha3(s: String): Array[Byte] = sha3(s.getBytes)

  def createNodeId(seed: String): NodeID = NodeID(sha3(seed))

  def newSetting(nodeID: NodeID, quorumSet: QuorumSet): Setting = Setting(
    localNodeID = nodeID,
    quorumSet = quorumSet,
    maxTimeoutSeconds = 30 * 60,
    connect = new TestConnect
  )
}
