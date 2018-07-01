package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcess[F[_]] extends NominationCore[F] {
  self: EnvelopeProcessHelper[F] with ModelSupport[F] with ConvenienceSupport[F] =>

  import model._

  /**
    * handle the message when received from self or peers
    *
    * @param envelope message envelope
    * @return
    */
  override def processEnvelope(nodeID: NodeID,
                               envelope: NominationEnvelope): SP[F, Envelope.State] = {
    val slotIndex = envelope.statement.slotIndex
    for {
      verified <- self.verifySignature(envelope)
      state <- if (!verified) invalidEnvelopeState
      else
        for {
          qSet                   <- nodeStore.getQuorumSet(nodeID)
          tracker                <- nodeStore.getNominateTracker(nodeID, slotIndex)
          trackerWithNewEnvelope <- self.saveNominationEnvelope(tracker, envelope)
          reduced                <- self.reduceNomination(trackerWithNewEnvelope, envelope.statement.message)
          valid                  <- self.validateNomination(nodeID, slotIndex, reduced)
          promoteAccept          <- self.promoteVotesToAccepted(trackerWithNewEnvelope, qSet, valid.voted)
          promoteCandidate       <- self.promoteAcceptedToCandidates(promoteAccept, qSet, valid.accepted)
          takenRoundLeadersOnDemand <- self.takeRoundLeadersVotesOnDemand(promoteCandidate,
                                                                          envelope.statement)
          promotedAccept    <- self.acceptedHasPromoted(tracker, takenRoundLeadersOnDemand)
          promotedCandidate <- self.candidatesHasPromoted(tracker, takenRoundLeadersOnDemand)
          emitted <- ifM[NominateTracker](promoteCandidate, _ => promotedAccept) { _ =>
            for {
              env <- nominateService.createNominationEnvelope(nodeID,
                                                              slotIndex,
                                                              qSet,
                                                              promoteCandidate.nomination)
              emit <- emitNominationMessage(nodeID,
                                            promoteCandidate,
                                            BoolResult(env, successful = true))
            } yield emit
          }
          bumped <- ifM[NominateTracker](emitted, _ => promotedCandidate)(_ =>
            bumpBallotState(nodeID, slotIndex, emitted))
          _ <- nodeStore.saveNominateTracker(nodeID, bumped)
        } yield Envelope.State.valid
    } yield state
  }
}
