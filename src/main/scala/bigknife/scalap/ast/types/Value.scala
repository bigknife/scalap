package bigknife.scalap.ast.types

trait Value extends Ordered[Value] {
  def orderFactor: Int
  def asBytes: Array[Byte]

  override def compare(that: Value): Int = this.orderFactor - that.orderFactor
}

object Value {
  trait Validity
  object Validity {
    case object FullyValidated extends Validity {
      override def toString: String = "FullyValidated"
    }
    case object Invalid extends Validity {
      override def toString: String = "Invalid"
    }
    case object MaybeValid extends Validity {
      override def toString: String = "MaybeValid"
    }
  }
}