package bigknife.scalap

import bigknife.scalap.ast.types.{Slot, StatementMessage, Value}
import bigknife.scalap.ast.usecase.SCP
import bigknife.scalap.ast.usecase.component.Model
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

trait SCPTest[F[_]] extends SCP[F] {

}

object SCPTest {
  def apply[F[_]](implicit M: Model[F]): SCPTest[F] = new SCPTest[F] {
    override val model: Model[F] = M
  }
}
