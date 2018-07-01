package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._
import bigknife.sop._
import bigknife.sop.implicits._

trait BallotBaseHelper[F[_]] {
  protected def emitCurrentStatement(tracker: BallotTracker): SP[F, Delta[BallotTracker]] = {
    ???
  }
}
