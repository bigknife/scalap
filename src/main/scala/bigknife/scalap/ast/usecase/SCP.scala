package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.{BallotBaseHelper, Bumping, BumpingHelper, EnvelopeProcessHelper => BEPH}
import bigknife.scalap.ast.usecase.component.Model
import bigknife.scalap.ast.usecase.nominate.{EnvelopeProcessHelper => NEPH, NominateHelper}


trait SCP[F[_]] extends NominationProtocol[F] with BallotProtocol[F] {
  self: NominateHelper[F]
    with NEPH[F]
    with BumpingHelper[F]
    with BEPH[F]
    with BallotBaseHelper[F]
    with ConvenienceSupport[F]
    with ModelSupport[F] =>
}

object SCP {
  def apply[F[_]](implicit M: Model[F]): SCP[F] =
    new SCP[F] with NominationProtocol[F] with BallotProtocol[F] with NominateHelper[F]
    with NEPH[F] with BEPH[F] with Bumping[F] with BumpingHelper[F] with BallotBaseHelper[F]
    with ConvenienceSupport[F] with ModelSupport[F] {
      override val model: Model[F] = M
    }
}