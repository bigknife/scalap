package bigknife.scalap

import bigknife.scalap.ast.types.{Slot, StatementMessage, Value}
import bigknife.scalap.ast.usecase.SCP
import bigknife.scalap.ast.usecase.component.Model
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

trait SCPTest[F[_]] extends SCP[F] {

  /**
    * verify a message in application level.
    *
    * @param message message with statement and signature
    * @return if passed true else false
    */
  override def verifyMessage(message: StatementMessage): SP[F, Boolean] = true.pureSP[F]

  /**
    * validate a nomination value's validity
    *
    * @param value value
    * @return
    */
  override def validateNominationValue(value: Value): SP[F, Value.Validity] =
    (Value.Validity.FullyValidated: Value.Validity).pureSP[F]

  override def validateBallotValue(value: Value): SP[F, Value.Validity] =
    (Value.Validity.FullyValidated: Value.Validity).pureSP[F]

  /**
    * try to transforms a value to a fully validted value that the local node would agree to
    *
    * @param slot  slot
    * @param value value(not fully validated)
    * @return
    */
  override def extractValidValue(slot: Slot, value: Value): SP[F, Option[Value]] =
    Option.empty[Value].pureSP[F]

  /**
    * emit message to other nodes
    *
    * @param message message
    * @return
    */
  override def emitMessage(message: StatementMessage): SP[F, Unit] = {
    println(s"emit message: $message")
    ().pureSP[F]
  }
}

object SCPTest {
  def apply[F[_]](implicit M: Model[F]): SCPTest[F] = new SCPTest[F] {
    override val model: Model[F] = M
  }
}
