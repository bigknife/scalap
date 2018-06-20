package bigknife.scalap.ast.types

case class Hash(value: Array[Byte]) {
  override def toString: String = s"Hash(${value.take(3).map("%02x" format _).mkString("")})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case Hash(that) => this.value sameElements that
    case _ => false
  }
}