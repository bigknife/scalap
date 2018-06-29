package bigknife.scalap.ast.types

/**
  * value, whatever data, represented by opaque bytes
  * @param bytes bytes
  */
case class Value(bytes: Array[Byte]) extends OpaqueBytes

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

    def fullyValidated: Validity = FullyValidated
    def invalid: Validity = Invalid
    def maybeValid: Validity = MaybeValid
  }
}