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
        _  <- logService.info(s"processed nomination message locally: $st", Some("nom-msg-emit"))
        ret <- if (st == Envelope.State.Valid) for {
          _ <- logService.info(s"start to broadcast envelope",
                               Some("nom-msg-proc"))
          needBroadcast <- nominateService.needBroadcastEnvelope(tracker, msgResult.data)
          nt <- if (needBroadcast) for {
            x <- nominateService.broadcastEnvelope(tracker, msgResult.data): SP[F, NominateTracker]
            _  <- logService.info(s"has broadcast envelope", Some("nom-msg-emit"))
          } yield x else for {
            _  <- logService.info(s"no need to broadcast envelope", Some("nom-msg-emit")): SP[F, Unit]
          } yield tracker
        } yield nt
        else for {
          _ <- logService.info("DOES NOT broadcast envelope, because invalid locally running result"): SP[F, Unit]
        } yield tracker
      } yield ret
    } else tracker.pureSP[F]
  }
}
