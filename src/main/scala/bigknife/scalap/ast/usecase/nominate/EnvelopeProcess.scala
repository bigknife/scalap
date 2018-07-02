package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._
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
  override def processNominationEnvelope(nodeID: NodeID,
                                         envelope: NominationEnvelope): SP[F, Envelope.State] = {
    val nodeID    = envelope.statement.nodeID
    val slotIndex = envelope.statement.slotIndex
    val statement = envelope.statement
    val message   = statement.message

    for {
      tracker  <- nodeStore.getNominateTracker(nodeID, slotIndex)
      verified <- self.verifySignature(envelope)
      state <- ifM[Envelope.State](
        Envelope.State.invalid,
        _ => verified && message.isSane && self.isNewerStatement(statement, tracker)) { _ =>
        for {
          qSet                   <- nodeStore.getQuorumSet(nodeID)
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
      }
    } yield state
  }
}
