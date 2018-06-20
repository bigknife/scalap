package bigknife.scalap.ast.store

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait QuorumSetStore[F[_]] {
  /**
    * get a quorum set from cache by hash
    * @param hash hash of quorum set
    * @return
    */
  def getQuorumSet(hash: Hash): P[F, Option[QuorumSet]]

  /**
    * save quorum set using hash as the key
    * @param hash hash of quorum set
    * @return
    */
  def saveQuorumSet(hash: Hash, quorumSet: QuorumSet): P[F, Unit]
}
