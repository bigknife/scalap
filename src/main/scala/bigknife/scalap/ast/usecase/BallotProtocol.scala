package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.{Bumping, BumpingHelper}

trait BallotProtocol[F[_]] extends Bumping [F]{
  self: ModelSupport[F] with ConvenienceSupport[F] with BumpingHelper[F] =>
}
