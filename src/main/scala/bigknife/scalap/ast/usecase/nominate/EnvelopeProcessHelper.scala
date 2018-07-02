package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.ballot.BallotCore
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport, NominateBaseHelper}
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcessHelper[F[_]] extends NominateBaseHelper[F] {
  self: NominationCore[F] with ModelSupport[F] with ConvenienceSupport[F] with BallotCore[F] =>

  import model._

  protected val invalidEnvelopeState: SP[F, Envelope.State] = Envelope.State.invalid.pureSP[F]

  /**
    * statement is newer than that saved in tracker
    * @param statement statement to be determined newer or not
    * @param tracker tracker
    * @return
    */
  def isNewerStatement(statement: Statement[Message.Nomination],
                       tracker: NominateTracker): Boolean = {
    !tracker.latestNominations.contains(statement.nodeID) ||
      statement.newerThan(
        tracker.latestNominations(statement.nodeID).statement)
  }

  /**
    * verify the signature of the envelope is valid.
    * @param envelope nomination message envelope
    * @return
    */
  protected def verifySignature(envelope: Envelope[Message.Nomination]): SP[F, Boolean] =
    envelopeService.verifyEnvelopeSignature(envelope)

  protected def saveNominationEnvelope(
      tracker: NominateTracker,
      envelope: Envelope[Message.Nomination]): SP[F, NominateTracker] =
    for {
      xTracker <- nominateService.recordEnvelope(tracker, envelope)
      _        <- nodeStore.saveHistoricalStatement(envelope.statement)
    } yield xTracker

  /**
    * reduce nomination, remove all values has been handled
    * @param tracker node nomination tracker
    * @param nomination nomination message from received envelope
    * @return unhandled nomination
    */
  protected def reduceNomination(tracker: NominateTracker,
                                 nomination: Message.Nomination): SP[F, Message.Nomination] = {
    val voted    = nomination.voted.filter(tracker.nomination.accepted.contain)
    val accepted = nomination.accepted.filter(tracker.candidates.contain)
    nomination
      .copy(
        voted = voted,
        accepted = accepted
      )
      .pureSP[F]
  }

  protected def validateNomination(nodeID: NodeID,
                                   slotIndex: SlotIndex,
                                   nomination: Message.Nomination): SP[F, Message.Nomination] = {
    // only necessary to validate votes
    val votedSP = nomination.voted.foldLeft(ValueSet.empty.pureSP[F]) { (acc, n) =>
      for {
        pre    <- acc
        passed <- nominateService.validateValue(n)
        next   <- (if (passed) pre + n else pre).pureSP[F]
      } yield next
    }

    val acceptedSP = nomination.accepted.pureSP[F]
    for {
      v <- votedSP
      a <- acceptedSP
    } yield Message.Nomination(v, a)
  }

  protected def promoteVotesToAccepted(tracker: NominateTracker,
                                       quorumSet: QuorumSet,
                                       votes: ValueSet): SP[F, NominateTracker] = {
    // if the nodes from which current received nominations that accepted the vote,
    // is vblocking of current node's vblocking, or a quorum of current node, then we accept it
    def votePredicate(v: Value): StatementPredicate[Message.Nomination] = {
      (nom: Statement[Message.Nomination]) =>
        nom.message.voted.contain(v)
    }
    def acceptPredicate(v: Value): StatementPredicate[Message.Nomination] = {
      (nom: NominationStatement) =>
        nom.message.accepted.contain(v)
    }

    // filter all federated accepted nodes, update them to the tracker
    // we should validate the accepted value, because they came from the outer.
    val acceptedVotes: SP[F, ValueSet] =
      votes.foldLeft(ValueSet.empty.pureSP[F]) { (acc, v) =>
        for {
          pre <- acc
          passedFA <- federatedAccept(v,
                                      quorumSet,
                                      tracker.latestNominations,
                                      votePredicate(v),
                                      acceptPredicate(v))
          passedV <- nominateService.validateValue(v)
        } yield if (passedFA && passedV) pre + v else pre
      }
    // update tracker's nomination, votes and accepted both should be updated
    def update(nomination: Message.Nomination, n: ValueSet): Message.Nomination = {
      nomination.copy(
        voted = nomination.voted ++ n,
        accepted = nomination.accepted ++ n
      )
    }
    acceptedVotes.map(x => tracker.copy(nomination = update(tracker.nomination, x)))
  }

  protected def takeRoundLeadersVotesOnDemand(
      tracker: NominateTracker,
      statement: NominationStatement): SP[F, NominateTracker] = {
    // if current candidates is empty, so we have to vote the outer votes
    if (tracker.candidates.isEmpty && tracker.roundLeaders.contains(statement.nodeID)) {
      val newNomVoted: SP[F, ValueSet] = for {
        newVoteOpt <- nominateService.getNewValueFromNomination(tracker, statement.message)
      } yield
        if (newVoteOpt.isDefined) tracker.nomination.voted + newVoteOpt.get
        else tracker.nomination.voted

      newNomVoted.map(x => tracker.copy(nomination = tracker.nomination.copy(voted = x)))
    } else tracker.pureSP[F]
  }

  protected def promoteAcceptedToCandidates(tracker: NominateTracker,
                                            quorumSet: QuorumSet,
                                            accepted: ValueSet): SP[F, NominateTracker] = {
    def acceptPredicate(v: Value): StatementPredicate[Message.Nomination] = {
      nom: NominationStatement =>
        nom.message.accepted.contain(v)
    }

    val candidatesSP: SP[F, ValueSet] = {
      accepted.foldLeft(ValueSet.empty.pureSP[F]) { (acc, v) =>
        for {
          pre    <- acc
          passed <- federatedRatify(v, quorumSet, tracker.latestNominations, acceptPredicate(v))
        } yield if (passed) pre + v else pre
      }
    }

    candidatesSP.map(x => tracker.copy(candidates = x))
  }

  protected def acceptedHasPromoted(oldTracker: NominateTracker,
                                    curTracker: NominateTracker): SP[F, Boolean] = {
    // if nomination has changed legally, true or false
    val oldNom = oldTracker.nomination
    val curNom = curTracker.nomination
    (curNom.voted.hasGrownFrom(oldNom.voted) || curNom.accepted.hasGrownFrom(oldNom.accepted))
      .pureSP[F]
  }

  protected def candidatesHasPromoted(oldTracker: NominateTracker,
                                      curTracker: NominateTracker): SP[F, Boolean] = {
    curTracker.candidates.hasGrownFrom(oldTracker.candidates).pureSP[F]
  }

  protected def bumpBallotState(nodeID: NodeID,
                                slotIndex: SlotIndex,
                                tracker: NominateTracker): SP[F, NominateTracker] = {
    for {
      combinedCandidate <- nominateService.combineValues(tracker.candidates)
      _                 <- bumpState(nodeID, slotIndex, combinedCandidate, force = false)
    } yield tracker
  }

}
