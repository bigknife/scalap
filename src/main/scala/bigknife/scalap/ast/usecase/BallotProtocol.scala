package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.Bumping

trait BallotProtocol[F[_]] extends Bumping [F]{

}
