package bigknife.scalap.ast.types

case class Node(
    id: Node.ID,
    isValidator: Boolean,
    quorumSet: QuorumSet
)

object Node {
  // let id of node as a hash
  type ID = Hash
}
