package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot._

trait BallotProtocol[F[_]] extends Bumping[F] with EnvelopeProcess[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BumpingHelper[F] with EnvelopeProcessHelper[F] with BallotBaseHelper[F] =>
}
