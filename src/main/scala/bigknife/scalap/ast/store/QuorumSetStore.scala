package bigknife.scalap.ast.store

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait QuorumSetStore[F[_]] {
  def getQuorumSet(hash: Hash): P[F, Option[QuorumSet]]
}
