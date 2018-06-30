package bigknife.scalap.ast.usecase.ballot
import bigknife.scalap.ast.types.Value
import bigknife.sop.SP

trait Bumping[F[_]] extends BallotCore[F] {

  /**
    * bump a candidate value(combined)
    *
    * @param value combined candidates
    * @param force force update ballot's tracker
    * @return
    */
  override def bumpState(value: Value, force: Boolean): SP[F, Unit] = ???
}
