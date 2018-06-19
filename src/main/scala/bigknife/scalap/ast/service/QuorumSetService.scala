package bigknife.scalap.ast.service

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait QuorumSetService[F[_]] {
  /**
    * create a singleton quorum set for a node
    * @param nodeId node id
    * @return
    */
  def buildSingletonQuorumSet(nodeId: Node.ID): P[F, QuorumSet]
  /**
    * Q(v) => a function for generating quorums for a node
    * @param nodeId node id
    * @return
    */
  def quorumFunction(nodeId: Node.ID): P[F, QuorumSet]

  /**
    * for a quorumset, is the given nodes a v-blocking set of it ?
    * @param quorumSet quorum set, Q(v)
    * @param nodes given node sets (id)
    * @return
    */
  def isVBlocking(quorumSet: QuorumSet, nodes: Vector[Node.ID]): P[F, Boolean]

  /**
    * for a quorumset, is the given nodes a quorum slice?
    * @param quorumSet
    * @param nodes
    * @return
    */
  def isQuorumSlice(quorumSet: QuorumSet, nodes: Vector[Node.ID]): P[F, Boolean]
}
