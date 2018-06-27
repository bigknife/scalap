package bigknife.scalap.ast.types

case class Signature(bytes: Array[Byte])

object Signature {
  val Empty: Signature = Signature(Array.emptyByteArray)
}
