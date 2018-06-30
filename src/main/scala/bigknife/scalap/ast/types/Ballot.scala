package bigknife.scalap.ast.types

case class Ballot(counter: Int, value: Value) extends Ordered[Ballot] {
  override def compare(that: Ballot): Int = {
    val c1 = this.counter - that.counter
    if (c1 == 0) {
      this.value.asHex().compare(that.value.asHex())
    } else c1
  }
}

object Ballot {}
