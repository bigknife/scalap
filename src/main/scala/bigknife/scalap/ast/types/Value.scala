package bigknife.scalap.ast.types

trait Value extends Ordered[Value] {
  def orderFactor: Long
  def asBytes: Array[Byte]

  override def compare(that: Value): Int = {
    val l = this.orderFactor - that.orderFactor

    def loop(l: Long): Long = if (l > Int.MaxValue.toLong) loop(l - Int.MaxValue.toLong) else l

    loop(l).toInt
  }
}

object Value {
  val Bottom: Value = new Value {
    override def orderFactor: Long = Long.MinValue
    override def asBytes: Array[Byte] = Array.emptyByteArray

    override def toString: String = "Value(Bottom)"
  }

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

    def fullyValidated: Validity = FullyValidated
    def invalid: Validity = Invalid
    def maybeValid: Validity = MaybeValid
  }

  trait ValueCombiner {
    def combine(values: Vector[Value]): Value
  }

  object ValueCombiner {
    def summon(f: Vector[Value] => Value): ValueCombiner = (values: Vector[Value]) => f(values)
  }
}