package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.component.Model

trait ModelSupport[F[_]] {
  val model: Model[F]
}
