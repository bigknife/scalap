package bigknife.scalap.ast.types

/**
  * value, whatever data, represented by opaque bytes
  */
trait Value extends OpaqueBytes with Ordered[Value] {
  def isEmpty: Boolean = this == Value.BottomValue
  def notEmpty: Boolean = !isEmpty
}

object Value {
  private object BottomValue extends Value {
    override def compare(that: Value): Int = if (that == this) 0 else -1
    override def bytes: Array[Byte]        = Array.emptyByteArray
  }

  private case class SimpleValue(data: Array[Byte]) extends Value {
    override def compare(that: Value): Int = that match {
      case BottomValue => 1
      case SimpleValue(dataThat) =>
        val thisStr = data.map("%02x" format _).mkString("")
        val thatStr = dataThat.map("%02x" format _).mkString("")
        thisStr.compareTo(thatStr)

      case _ => -1
    }

    override def bytes: Array[Byte] = data
  }

  def bottom: Value = BottomValue

  def simple(data: Array[Byte]): Value = SimpleValue(data)
  def apply(data: Array[Byte]): Value = simple(data)

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
    def invalid: Validity        = Invalid
    def maybeValid: Validity     = MaybeValid
  }
}
