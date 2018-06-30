package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.usecase.ballot.Bumping
import bigknife.scalap.ast.usecase.component.Model
import bigknife.scalap.ast.usecase.nominate.{EnvelopeProcessHelper, NominateHelper}

trait SCP[F[_]] extends NominationProtocol[F] with BallotProtocol[F] {
  self: NominateHelper[F] with EnvelopeProcessHelper[F] with ModelSupport[F] =>
}

object SCP {
  def apply[F[_]](implicit M: Model[F]): SCP[F] =
    new SCP[F] with NominationProtocol[F] with BallotProtocol[F] with NominateHelper[F]
    with EnvelopeProcessHelper[F] with Bumping[F] with ModelSupport[F] {
      override val model: Model[F] = M
    }
}
