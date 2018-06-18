package bigknife.scalap.ast.types

trait Value extends Ordered[Value] {
  def orderFactor: Int
  override def compare(that: Value): Int = this.orderFactor - that.orderFactor
}