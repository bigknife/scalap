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
        _  <- nodeStore.saveNominateTracker(nodeID, tracker)
        st <- self.processNominationEnvelope(nodeID, msgResult.data)
        _  <- logService.info(s"processed nomination message locally: $st", Some("nom-msg-proc"))
        ret <- if (st == Envelope.State.Valid) for {
          _ <- logService.info(s"start to broadcast envelope: ${msgResult.data}",
                               Some("nom-msg-proc"))
          nt <- nominateService.broadcastEnvelope(tracker, msgResult.data): SP[F, NominateTracker]
          _  <- logService.info(s"broadcasted envelope: ${msgResult.data}", Some("nom-msg-proc"))
        } yield nt
        else tracker.pureSP[F]
      } yield ret
    } else tracker.pureSP[F]
  }
}
