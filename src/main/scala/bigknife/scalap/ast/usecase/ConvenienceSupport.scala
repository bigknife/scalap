package bigknife.scalap.ast.usecase

import bigknife.sop._
import bigknife.sop.implicits._

trait ConvenienceSupport[F[_]] {

  def ifM[A](data: A, p: A => Boolean)(spf: A => SP[F, A]): SP[F, A] =
    if(p(data)) spf(data) else data.pureSP[F]

}
