package bigknife.scalap.ast.types

case class Signature(bytes: Array[Byte]) extends OpaqueBytes

object Signature {
  def empty: Signature = Signature(Array.emptyByteArray)
}
