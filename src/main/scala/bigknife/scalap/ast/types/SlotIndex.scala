package bigknife.scalap.ast.types

case class SlotIndex(index: BigInt) extends OpaqueBytes {
  def >(that: SlotIndex): Boolean = this.index > that.index
  def ==(that: SlotIndex): Boolean = this.index == that.index
  def <(that: SlotIndex): Boolean = this.index < that.index

  override def bytes: Array[Byte] = index.toByteArray
}
object SlotIndex {
  def apply(i: Int): SlotIndex = SlotIndex(BigInt(i))
  def apply(i: Long): SlotIndex = SlotIndex(BigInt(i))
  def apply(i: String, radix: Int = 10): SlotIndex = SlotIndex(BigInt.apply(i, radix))
}
