package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast
import bigknife.scalap.ast.types
import bigknife.scalap.ast.types.BallotTracker.Phase
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcessHelper[F[_]] extends BallotBaseHelper[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BallotCore[F] =>
  import model._

  /**
    * statement is newer than that saved in tracker
    * @param statement statement to be determined newer or not
    * @param tracker tracker
    * @return
    */
  def isNewerStatement[M <: BallotMessage](statement: BallotStatement[M],
                                           tracker: BallotTracker): Boolean = {
    val old: BallotEnvelope[BallotMessage] = tracker.latestBallotEnvelope(statement.nodeID)

    !tracker.latestBallotEnvelope.contains(statement.nodeID) || Statement.newerThan(old.statement,
                                                                                    statement)

  }

  /**
    * validate values of ballot statement
    * @param statement ballot message statement
    * @return
    */
  def validateValues[M <: BallotMessage](
      statement: Statement.BallotStatement[M]): SP[F, Boolean] = {
    statement match {
      case x: Statement.Prepare =>
        val msg: Message.Prepare = x.message
        for {
          b <- envelopeService.validateValue(msg.ballot.value)
          p <- ifM[Boolean](true, _ => msg.prepared.value.notEmpty) { _ =>
            envelopeService.validateValue(msg.prepared.value)
          }
        } yield b && p
      case x: Statement.Commit =>
        envelopeService.validateValue(x.message.ballot.value)
      case x: Statement.Externalize =>
        envelopeService.validateValue(x.message.commit.value)
    }
  }

  /**
    * get candidates ballots for prepare phase
    * @param tracker
    * @param statement
    * @tparam M
    * @return
    */
  def getPreparedCandidateBallots[M <: BallotMessage](
      tracker: BallotTracker,
      statement: BallotStatement[M]): Set[Ballot] = {
    // get ballots from statement, if the statement is commit or externalize, let the ballot value be
    // max, so it can be used to advance local state
    val hintBallots: Set[Ballot] = statement.message match {
      case x: Message.Prepare =>
        Set(x.ballot, x.prepared, x.preparedPrime).filter(_.notNull)
      case x: Message.Commit =>
        Set(Ballot(x.preparedCounter, x.ballot.value), Ballot.max(x.ballot.value))
      case x: Message.Externalize =>
        Set(Ballot.max(x.commit.value))
    }

    // then find ballot from tracker's latest message.
    tracker.latestBallotEnvelope.foldLeft(Set.empty[Ballot]) {
      case (acc, (_, envelope)) =>
        envelope.statement match {
          case x: Statement.Prepare =>
            acc ++ Set(x.message.ballot, x.message.prepared, x.message.preparedPrime).filter { b =>
              b.notNull && hintBallots.exists(b.lessAndCompatible)
            }
          case x: Statement.Commit =>
            val b  = x.message.ballot
            val s1 = if (hintBallots.exists(b.compatible)) Set(b) else Set()
            acc ++ s1 ++ hintBallots
              .find(h => x.message.preparedCounter < h.counter)
              .map(h => Ballot(x.message.preparedCounter, h.value))
          case x: Statement.Externalize =>
            acc ++ hintBallots.find(x.message.commit.compatible)
        }

    }

  }

  private def attemptPreparedAccept[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    //if not in prepare or confirm phase, ignore it.
    if (tracker.phase == Phase.Externalize) Delta.unchanged(tracker).pureSP[F]
    else {
      // we should do some filtering work. to reduce the ballots that can't help local to advance.
      val candidates: Vector[Ballot] = getPreparedCandidateBallots(tracker, hint).toVector.filter {
        b =>
          val cond1 = tracker.isCommitPhase && !tracker.prepared.lessAndCompatible(b)
          val cond2 = tracker.preparedPrimeBallotNotNull && (b <= tracker.preparedPrime)
          val cond3 = tracker.preparedBallotNotNull && b.lessAndCompatible(tracker.prepared)
          !(cond1 || cond2 || cond3)
      }.sorted

      // now, find a passed federated-accepted ballot to advance local
      val BREAK = true
      val NOT_BREAK = false

      def votedPredict(ballot: Ballot): StatementPredicate[Message.BallotMessage] = { x =>
        x.message match {
          case m: Message.Prepare => ballot.lessAndCompatible(m.ballot)
          case m: Message.Commit => ballot.compatible(m.ballot)
          case m: Message.Externalize => ballot.compatible(m.commit)
        }
      }

      def acceptedPredict(ballot: Ballot): StatementPredicate[Message.BallotMessage] = { x =>
        x.message match {
          case m: Message.Prepare =>
            (m.prepared.notNull && ballot.lessAndCompatible(m.prepared)) ||
              (m.preparedPrime.notNull && ballot.lessAndCompatible(m.preparedPrime))
          case m: Message.Commit =>
            val prepared = Ballot(m.preparedCounter, m.ballot.value)
            ballot.lessAndCompatible(prepared)
          case m: Message.Externalize =>
            ballot.compatible(m.commit)
        }
      }

      val acceptedOptSP: SP[F, Option[Ballot]] =
        candidates
          .foldRight((Option.empty[Ballot], NOT_BREAK).pureSP[F]) { (n, acc) =>
            for {
              pre <- acc
              x <- ifM[(Option[Ballot], Boolean)](pre, !_._2) { _ =>
                for {
                  passed <- federatedAccept(quorumSet,
                    tracker.latestBallotEnvelope,
                    votedPredict(n),
                    acceptedPredict(n))
                } yield if (passed) (Option(n), BREAK) else pre
              }
            } yield pre
          }
          .map(_._1)

      // if acceptedOpt is defined try to accept prepared
      for {
        acceptedOpt <- acceptedOptSP
        trackerD <- ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _ => acceptedOpt.isDefined) {
          _ =>
            for {
              trackerD0 <- ballotService.setPrepared(tracker, acceptedOpt.get)
              trackerD1 <- ballotService.clearCommitIfNeeded(trackerD0.data)
            } yield Delta(trackerD1.data, trackerD1.changed || trackerD0.changed)
        }
        trackerFinal <- ifM[Delta[BallotTracker]](trackerD, _ => trackerD.changed) { x =>
          emitCurrentStateStatement(x.data, quorumSet)
        }
      } yield trackerFinal
    }
  }

  private def attemptPreparedConfirm[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    ???
  }

  private def attemptCommitAccept[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    ???
  }

  private def attemptCommitConfirm[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    ???
  }

  /**
    * advance the slot
    * @param statement
    * @param tracker
    * @return
    */
  def advanceSlot[M <: BallotMessage](tracker: BallotTracker,
                                      quorumSet: QuorumSet,
                                      statement: BallotStatement[M]): SP[F, BallotTracker] = {
    //todo: attempt to bump state
    //todo: send latest envelope(if not sent)
    for {
      pa <- attemptPreparedAccept(tracker, quorumSet, statement)
      pc <- attemptPreparedConfirm(pa.data, quorumSet, statement)
      ca <- attemptCommitAccept(pc.data, quorumSet, statement)
      cc <- attemptCommitConfirm(ca.data, quorumSet, statement)
    } yield cc.data
  }

  def recordEnvelope[M <: BallotMessage](tracker: BallotTracker,
                                         envelope: BallotEnvelope[M]): SP[F, BallotTracker] =
    for {
      t <- ballotService.recordEnvelope(tracker, envelope)
      _ <- nodeStore.saveHistoricalStatement(envelope.statement)
    } yield t

  def getWorkingBallot[M <: BallotMessage](statement: BallotStatement[M]): SP[F, Ballot] = {
    statement.message match {
      case x: Message.Prepare     => x.ballot.pureSP[F]
      case x: Message.Commit      => Ballot(x.cCounter, x.ballot.value).pureSP[F]
      case x: Message.Externalize => x.commit.pureSP[F]
    }
  }



}
