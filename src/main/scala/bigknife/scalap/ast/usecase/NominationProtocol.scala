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