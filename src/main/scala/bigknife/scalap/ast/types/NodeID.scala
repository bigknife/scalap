package bigknife.scalap.ast.types

/**
  * NodeID
  * @param bytes opaque bytes
  */
case class NodeID(bytes: Array[Byte]) extends OpaqueBytes
object NodeID {
  def empty: NodeID = NodeID(Array.emptyByteArray)
}
