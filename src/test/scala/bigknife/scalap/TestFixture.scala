package bigknife.scalap

import bigknife.scalap.ast.usecase.SCP
import bigknife.scalap.ast.usecase.component._

trait TestFixture {
  val scp: SCP[Model.Op] = SCP[Model.Op]
}
