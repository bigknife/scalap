package bigknife.scalap.ast.types

trait OpaqueBytes {
  def bytes: Array[Byte]

  override def equals(obj: scala.Any): Boolean = obj match {
    case x: OpaqueBytes if x.getClass == obj.getClass => x.bytes sameElements this.bytes
    case _ => false
  }

  /** is the bytes equal */
  def ===(that: OpaqueBytes): Boolean = this.bytes sameElements that.bytes

  def asHex(upper: Boolean = false): String = {
    val fmt = if (upper) "%02X" else "%02x"
    bytes.map(fmt format _).mkString("")
  }

  override def toString: String = {
    val cls = getClass.getSimpleName
    // hex, at most 4 ...
    val hex = asHex()
    val expr = if (hex.length >= 7) hex.take(4) + "..." else hex
    s"$cls(0x$expr)"
  }
}
