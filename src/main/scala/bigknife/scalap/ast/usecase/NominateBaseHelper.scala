package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.nominate.NominationCore
import bigknife.sop._
import bigknife.sop.implicits._

trait NominateBaseHelper[F[_]] {
  self: NominationCore[F] with ModelSupport[F] with ConvenienceSupport[F] =>
  import model._

  protected def emitNominationMessage(
      nodeID: NodeID,
      tracker: NominateTracker,
      msgResult: NominationEnvelopeResult): SP[F, NominateTracker] = {
    if (msgResult.successful) {
      for {
        st <- self.processNominationEnvelope(nodeID, msgResult.data)
        ret <- if (st == Envelope.State.Valid) for {
          nt <- nominateService.broadcastEnvelope(tracker, msgResult.data): SP[F, NominateTracker]
        } yield nt
        else tracker.pureSP[F]
      } yield ret
    } else tracker.pureSP[F]
  }
}
