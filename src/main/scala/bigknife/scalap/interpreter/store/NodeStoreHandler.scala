package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.NodeStore
import bigknife.scalap.ast.types.Node
import bigknife.scalap.ast.types.Node.ID

class NodeStoreHandler extends NodeStore.Handler[Stack] {
  import scala.collection._
  private val nodeStore: mutable.Map[Node.ID, Node] = mutable.Map.empty

  override def findNode(nodeId: ID): Stack[Option[Node]] = Stack {
    nodeStore.get(nodeId)
  }
}

object NodeStoreHandler {
  private val _inst = new NodeStoreHandler
  trait Implicits {
    implicit val nodeStoreHandler: NodeStoreHandler = _inst
  }
}
