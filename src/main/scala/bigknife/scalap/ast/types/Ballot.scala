package bigknife.scalap.ast.types

case class Ballot(counter: Int, value: Value) extends Ordered[Ballot] {
  override def compare(that: Ballot): Int = {
    val c1 = this.counter - that.counter
    if (c1 == 0) {
      this.value.asHex().compare(that.value.asHex())
    } else c1
  }

  def isNull: Boolean = this == Ballot.Null

  def compatible(that: Ballot): Boolean          = this.value == that.value
  def incompatible(that: Ballot): Boolean        = this.value != that.value
  def lessAndCompatible(that: Ballot): Boolean   = this <= that && compatible(that)
  def lessAndIncompatible(that: Ballot): Boolean = this <= that && incompatible(that)
}

object Ballot {
  val Null: Ballot = Ballot(0, Value.bottom)
}
