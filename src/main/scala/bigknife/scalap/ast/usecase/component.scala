package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.service._
import bigknife.scalap.ast.store._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

/**
  * system component
  */
object component {
  @sps trait Model[F[_]] {
    val logService: LogService[F]
    val nominateService: NominateService[F]
    val nodeStore: NodeStore[F]
    val ballotService: BallotService[F]
    val envelopeService: EnvelopeService[F]
  }
}
