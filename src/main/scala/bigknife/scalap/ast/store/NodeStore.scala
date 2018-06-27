package bigknife.scalap.ast.store

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait NodeStore[F[_]] {
  /**
    * find a node of id
    * @param nodeId node id
    * @return
    */
  def findNode(nodeId: Node.ID): P[F, Option[Node]]
}
