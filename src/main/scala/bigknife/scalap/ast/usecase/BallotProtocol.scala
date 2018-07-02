package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.{BallotBaseHelper, Bumping, BumpingHelper}

trait BallotProtocol[F[_]] extends Bumping[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BumpingHelper[F] with BallotBaseHelper[F] =>
}
