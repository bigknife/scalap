package bigknife.scalap
package ast.types

import java.nio.ByteBuffer

/**
  * quorum slices k-of-n, threshold is K, and size is N
  */
sealed trait QuorumSet {
  def threshold: Int
  def size: Int

  /**
    * nest a new quorum set
    * @param threshold threshold
    * @param nodeIds node ids
    * @return
    */
  def nest(threshold: Int, nodeIds: NodeID*): QuorumSet =
    QuorumSet.nest(this, threshold, nodeIds: _*)

  /**
    * compute a node weight in this quorumset
    * @param nodeID node id
    * @return weight
    */
  def nodeWeight(nodeID: NodeID): Long = QuorumSet.nodeWeight(this, nodeID)

  def allNodes: Set[NodeID] = QuorumSet.allNodes(this)

  def neighbors(round: Int, slotIndex: SlotIndex, previousValue: Value): Set[NodeID] =
    QuorumSet.neighbors(this, round, slotIndex, previousValue)

  lazy val hash: Hash = QuorumSet.hash(this)

  /**
    * compute if the nodeIDs is current quorumset's vblocking set
    * @param nodeIDs nodes
    * @return
    */
  def isVBlocking(nodeIDs: Set[NodeID]): Boolean = QuorumSet.isVBlocking(this, nodeIDs)

  /**
    * compute if the nodeIDs is current quorumset's quorum slice
    * @param nodeIDs nodes
    * @return
    */
  def isQuorumSlice(nodeIDs: Set[NodeID]): Boolean = QuorumSet.isQuorumSlice(this, nodeIDs)
}
object QuorumSet {
  case class Simple(
      threshold: Int,
      validators: Set[NodeID]
  ) extends QuorumSet {
    override def size: Int = validators.size
  }
  case class Nest(
      threshold: Int,
      validators: Set[NodeID],
      innerSets: Set[Simple]
  ) extends QuorumSet {
    override def size: Int = validators.size + innerSets.size
  }

  def fake: QuorumSet = Simple(0, Set.empty)

  def simple(threshold: Int, nodeIds: NodeID*): QuorumSet =
    Simple(threshold, nodeIds.toSet)

  def singleton(nodeID: NodeID): QuorumSet = simple(1, nodeID)

  def nest(quorumSet: QuorumSet, threshold: Int, nodeIds: NodeID*): QuorumSet = quorumSet match {
    case Simple(_threshold, validators) =>
      Nest(_threshold, validators, Set(simple(threshold, nodeIds: _*).asInstanceOf[Simple]))
    case x: Nest =>
      x.copy(innerSets = x.innerSets + simple(threshold, nodeIds: _*).asInstanceOf[Simple])
  }

  def nodeWeight(quorumSet: QuorumSet, nodeID: NodeID): Long = {
    quorumSet match {
      case x @ Simple(threshold, validators) =>
        if (validators.contains(nodeID)) {
          (BigInt(Long.MaxValue) * BigInt(threshold) / BigInt(x.size)).toLong
        } else 0L

      case x @ Nest(threshold, validators, innerSets) =>
        if (validators.contains(nodeID)) {
          (BigInt(Long.MaxValue) * BigInt(threshold) / BigInt(x.size)).toLong
        } else {
          innerSets
            .find(_.validators.contains(nodeID))
            .map { qs =>
              val leafW = nodeWeight(qs, nodeID)
              (BigInt(leafW) * BigInt(threshold) / BigInt(x.size)).toLong
            }
            .getOrElse(0)
        }
    }
  }

  def allNodes(quorumSet: QuorumSet): Set[NodeID] = quorumSet match {
    case Simple(_, validators) => validators
    case Nest(_, validators, innerSets) =>
      validators ++ innerSets.foldLeft(Set.empty[NodeID]) { (acc, n) =>
        acc ++ n.validators
      }
  }

  def neighbors(quorumSet: QuorumSet,
                round: Int,
                slotIndex: SlotIndex,
                previousValue: Value): Set[NodeID] = {
    allNodes(quorumSet).filter({ nodeID =>
      val w = nodeWeight(quorumSet, nodeID)
      util.gi(util.Gi.Mode.Mode_Neighbor, nodeID, round, slotIndex, previousValue) < w
    })
  }

  def hash(quorumSet: QuorumSet): Hash = {
    def hashSimple(simple: Simple): Hash =
      Hash(
        util.crypoto.sha3(
          (simple.threshold.toString.getBytes.toVector ++
            simple.validators
              .map(_.asHex())
              .toVector
              .sorted
              .flatMap(_.getBytes.toVector)).toArray))

    quorumSet match {
      case x: Simple => hashSimple(x)
      case Nest(threshold, validators, innerSets) =>
        Hash(
          (threshold.toString.getBytes.toVector ++ validators
            .map(_.asHex())
            .toVector
            .sorted
            .flatMap(_.getBytes.toVector) ++ innerSets
            .map(hashSimple)
            .map(_.asHex())
            .toVector
            .sorted
            .flatMap(_.getBytes.toVector)).toArray)
    }
  }

  def isVBlocking(quorumSet: QuorumSet, nodeIDs: Set[NodeID]): Boolean = {
    if (quorumSet.threshold == 0) false
    else {
      val leftTillBlock = quorumSet.size - quorumSet.threshold + 1
      quorumSet match {
        case Simple(_, validators) =>
          validators.count(nodeIDs.contains) >= leftTillBlock
        case Nest(_, validators, innerSets) =>
          val rest = leftTillBlock - validators.count(nodeIDs.contains)
          if (rest <= 0) true
          else {
            val c = innerSets.foldLeft(0) { (acc, n) =>
              val vb = isVBlocking(n, nodeIDs)
              if (vb) acc + 1 else acc
            }
            rest - c <= 0
          }
      }
    }
  }

  def isQuorumSlice(quorumSet: QuorumSet, nodeIDs: Set[NodeID]): Boolean =
    if (quorumSet.threshold == 0) false
    else
      quorumSet match {
        case Simple(threshold, validators) =>
          validators.count(nodeIDs.contains) >= threshold
        case Nest(threshold, validators, innerSets) =>
          val rest = threshold - validators.count(nodeIDs.contains)
          if (rest <= 0) true
          else {
            val c = innerSets.foldLeft(0) { (acc, n) =>
              if (isQuorumSlice(n, nodeIDs)) acc + 1 else acc
            }
            rest - c <= 0
          }
      }
}
