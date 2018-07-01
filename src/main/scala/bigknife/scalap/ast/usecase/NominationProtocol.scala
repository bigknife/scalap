package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.{BallotBaseHelper, Bumping, BumpingHelper}
import bigknife.scalap.ast.usecase.component.Model
import bigknife.scalap.ast.usecase.nominate._

trait NominationProtocol[F[_]] extends Nominating[F] with EnvelopeProcess[F] {
  self: NominateHelper[F]
    with EnvelopeProcessHelper[F]
    with ModelSupport[F]
    with ConvenienceSupport[F] =>
}

object NominationProtocol {
  def apply[F[_]](implicit M: Model[F]): NominationProtocol[F] =
    new NominationProtocol[F] with NominateHelper[F] with EnvelopeProcessHelper[F] with Bumping[F]
    with ConvenienceSupport[F] with BumpingHelper[F] with BallotBaseHelper[F] with ModelSupport[F] {
      override val model: Model[F] = M
    }
}
