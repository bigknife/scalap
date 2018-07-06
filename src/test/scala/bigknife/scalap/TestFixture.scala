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

  def newSetting(nodeID: NodeID, quorumSet: QuorumSet, presetQuorumSets: Map[NodeID, QuorumSet], connect: Connect): Setting = Setting(
    localNodeID = nodeID,
    quorumSet = quorumSet,
    maxTimeoutSeconds = 30 * 60,
    connect = connect,
    presetQuorumSets = presetQuorumSets
  )

  case class SCPCluster(
      nodeIDs: Vector[NodeID],
      quorumSets: Map[NodeID, QuorumSet]
  ) {
    def getNode(idx: Int): NodeID         = nodeIDs(idx)
    def getQuorumSet(idx: Int): QuorumSet = quorumSets(getNode(idx))
    def createNominationEnvelope(ownerIdx: Int,
                                 slotIndex: SlotIndex,
                                 vote: Option[TestValue],
                                 accept: Option[TestValue]): Envelope.NominationEnvelope = {
      val nb = Message.nominationBuilder()
      vote.foreach(nb.vote(_))
      accept.foreach(nb.accept(_))

      val nom = Statement.Nominate(getNode(ownerIdx), slotIndex, getQuorumSet(ownerIdx).hash, nb.build())
      Envelope.NominationEnvelope(nom, Signature.empty)
    }

  }

  lazy val simple4nodes: SCPCluster = {
    val nodeIDs = (0 to 3).toVector.map(x => "v" + x).map(createNodeId)
    val qs      = QuorumSet.simple(3, nodeIDs: _*)
    val quorumSets = nodeIDs.foldLeft(Map.empty[NodeID, QuorumSet]) { (acc, n) =>
      acc + (n -> qs)
    }
    SCPCluster(nodeIDs, quorumSets)
  }
}
