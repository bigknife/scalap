package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.{BallotBaseHelper, Bumping, BumpingHelper, EnvelopeProcess}

trait BallotProtocol[F[_]] extends Bumping[F] with EnvelopeProcess[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BumpingHelper[F] with EnvelopeProcess[F] with BallotBaseHelper[F] =>
}
