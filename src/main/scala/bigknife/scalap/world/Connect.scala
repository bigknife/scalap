package bigknife.scalap.world

import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._

/**
  * connected to outer world
  */
trait Connect {

  /**
    * try to extract a valid value from a not full validated value
    * @param value value not full validated
    * @return if possible, Some extracted value, or None
    */
  def extractValidValue(value: Value): Option[Value]

  /**
    * application level validation
    * @param value value
    * @return full/ maybe/ invalid
    */
  def validateValue(value: Value): Value.Validity
}

object Connect {
  def dummy: Connect = new Connect {
    /**
      * try to extract a valid value from a not full validated value
      *
      * @param value value not full validated
      * @return if possible, Some extracted value, or None
      */
    override def extractValidValue(value: Value): Option[Value] = None

    /**
      * application level validation
      *
      * @param value value
      * @return full/ maybe/ invalid
      */
    override def validateValue(value: Value): Value.Validity = Validity.fullyValidated
}
}