package bigknife.scalap.ast.types

case class Ballot(
    counter: Int,
    value: Value
) extends Ordered[Ballot] {

  override def compare(that: Ballot): Int = {
    val c = this.counter - that.counter
    if (c != 0) c
    else this.value.compare(that.value)
  }

  // see the paper 6.2
  def lessThan(that: Ballot): Boolean     = this.counter <= that.counter
  def compatible(that: Ballot): Boolean   = this.value == that.value
  def incompatible(that: Ballot): Boolean = this.value != that.value

  def lessThanAndIncompatible(that: Ballot): Boolean = lessThan(that) && incompatible(that)
  def lessThanAndCompatible(that: Ballot): Boolean   = lessThan(that) && compatible(that)

}
