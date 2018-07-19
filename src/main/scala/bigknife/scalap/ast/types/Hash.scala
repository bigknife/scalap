package bigknife.scalap.ast.types

case class Hash(bytes: Array[Byte]) extends OpaqueBytes {
  override def equals(obj: Any): Boolean = obj match {
    case Hash(thatBytes) => thatBytes sameElements bytes
    case _ => false
  }

  override def hashCode(): Int = {
    java.util.Objects.hash(asHex() + getClass)
  }
}
