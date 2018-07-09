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
    val remoteNodeID = envelope.statement.nodeID
    val slotIndex    = envelope.statement.slotIndex
    val statement    = envelope.statement
    val message      = statement.message

    for {
      tracker  <- nodeStore.getNominateTracker(nodeID, slotIndex)
      verified <- self.verifySignature(envelope)
      _ <- logService.debug(s"nomination envelope signature verified ? $verified",
                            Some("nom-msg-proc"))
      sane  <- message.isSane(nodeID == remoteNodeID).pureSP[F]
      _     <- logService.debug(s"nomination message sane ? $sane", Some("nom-msg-proc"))
      newer <- self.isNewerStatement(statement, tracker).pureSP[F]
      _     <- logService.debug(s"nomination statement newer ? $newer", Some("nom-msg-proc"))
      state <- ifM[Envelope.State](Envelope.State.invalid, _ => verified && sane && newer) { _ =>
        for {
          qSet                   <- nodeStore.getQuorumSet(nodeID)
          trackerWithNewEnvelope <- self.saveNominationEnvelope(tracker, envelope)
          reduced                <- self.reduceNomination(trackerWithNewEnvelope, envelope.statement.message)
          _                      <- logService.info(s"reduced nomination message: $reduced", Some("nom-msg-proc"))
          valid                  <- self.validateNomination(nodeID, slotIndex, reduced)
          promoteAccept          <- self.promoteVotesToAccepted(trackerWithNewEnvelope, qSet, valid.voted)
          _ <- logService.info(s"after promote to accept: ${promoteAccept.nomination}",
                               Some("nom-msg-proc"))
          promoteCandidate <- self.promoteAcceptedToCandidates(promoteAccept, qSet, valid.accepted)
          _ <- logService.info(s"after promote to candidate: ${promoteAccept.nomination}",
                               Some("nom-msg-proc"))
          takenRoundLeadersOnDemand <- self.takeRoundLeadersVotesOnDemand(promoteCandidate,
                                                                          envelope.statement)
          _ <- logService.info(
            s"after takenRoundLeadersOnDemand: ${takenRoundLeadersOnDemand.nomination}",
            Some("nom-msg-proc"))
          promotedAccept    <- self.acceptedHasPromoted(tracker, takenRoundLeadersOnDemand)
          _                 <- logService.info(s"has promoted accepted? $promotedAccept", Some("nom-msg-proc"))
          promotedCandidate <- self.candidatesHasPromoted(tracker, takenRoundLeadersOnDemand)
          _                 <- logService.info(s"has promoted candidate? $promotedCandidate", Some("nom-msg-proc"))
          emitted <- ifM[NominateTracker](takenRoundLeadersOnDemand, _ => promotedAccept) { _ =>
            for {
              env <- nominateService.createNominationEnvelope(nodeID,
                                                              slotIndex,
                                                              qSet,
                                                              promoteCandidate.nomination)
              _ <- logService.info(s"try to emit nomination message: $env", Some("nom-msg-proc"))
              emit <- emitNominationMessage(nodeID,
                                            promoteCandidate,
                                            BoolResult(env, successful = true))
            } yield emit
          }
          bumped <- ifM[NominateTracker](emitted, _ => promotedCandidate) { _ =>
            for {
              _ <- logService.debug(
                s"start to bump, nodeID=$nodeID, slotIndex=$slotIndex, emitted=$emitted", Some("nom-msg-proc"))
              x <- bumpBallotState(nodeID, slotIndex, emitted)
              _ <- logService.debug(
                s"after bumping, nodeID=$nodeID, slotIndex=$slotIndex, emitted=$emitted", Some("nom-msg-proc"))
            } yield x
          }
          _ <- logService.info(s"after bumped: ${bumped.nomination}", Some("nom-msg-proc"))
          _ <- nodeStore.saveNominateTracker(nodeID, bumped)
        } yield Envelope.State.valid
      }
    } yield state
  }
}
