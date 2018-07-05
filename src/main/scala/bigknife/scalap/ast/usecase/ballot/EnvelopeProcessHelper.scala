package bigknife.scalap.ast.usecase.ballot

import bigknife.scalap.ast.types.BallotTracker.Phase
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.implicits._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport}
import bigknife.sop._
import bigknife.sop.implicits._

trait EnvelopeProcessHelper[F[_]] extends BallotBaseHelper[F] {
  self: ModelSupport[F] with ConvenienceSupport[F] with BallotCore[F] with BumpingHelper[F] =>
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
    * @param tracker tracker
    * @param statement statement
    * @return
    */
  private def getPreparedCandidateBallots[M <: BallotMessage](
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

  private def getAcceptCommitBallots[M <: BallotMessage](tracker: BallotTracker,
                                                         statement: BallotStatement[M]): Ballot = {
    statement.message match {
      case x: Message.Prepare =>
        if (x.cCounter != 0) Ballot(x.hCounter, x.ballot.value) else Ballot.Null
      case x: Message.Commit      => Ballot(x.hCounter, x.ballot.value)
      case x: Message.Externalize => Ballot(x.hCounter, x.commit.value)
    }
  }

  private def attemptPreparedAccept[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    //if not in prepare or confirm phase, ignore it.
    if (tracker.isExternalizePhase) Delta.unchanged(tracker).pureSP[F]
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
      val BREAK     = true
      val NOT_BREAK = false

      def votedPredict(ballot: Ballot): StatementPredicate[Message.BallotMessage] = { x =>
        x.message match {
          case m: Message.Prepare     => ballot.lessAndCompatible(m.ballot)
          case m: Message.Commit      => ballot.compatible(m.ballot)
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
            } yield x
          }
          .map(_._1)

      // if acceptedOpt is defined try to accept prepared
      for {
        acceptedOpt <- acceptedOptSP
        trackerD <- ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _ => acceptedOpt.isDefined) {
          _ =>
            for {
              trackerD0 <- ballotService.setPreparedAccepted(tracker, acceptedOpt.get)
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
    // if not in prepare phase, ignore it
    // if not accepted prepared, can't to confirm it
    if (tracker.notPreparePhase || tracker.preparedBallotIsNull) Delta.unchanged(tracker).pureSP[F]
    else {
      val candidates = getPreparedCandidateBallots(tracker, hint).toVector.sorted

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
      val BREAK    = true
      val NO_BREAK = false
      val newHightBallotOptSP: SP[F, Option[Ballot]] =
        candidates
          .foldRight((Option.empty[Ballot], NO_BREAK).pureSP[F]) { (n, acc) =>
            // if local high is ge than all other nodes accepted, passed
            val isBreak = tracker.highBallotNotNull && tracker.high >= n
            for {
              pre <- acc
              x <- ifM[(Option[Ballot], Boolean)](pre, !_._2 && !isBreak) { _ =>
                for {
                  ratified <- federatedRatify(quorumSet,
                                              tracker.latestBallotEnvelope,
                                              acceptedPredict(n))
                } yield if (ratified) (Option(n), BREAK) else pre
              }
            } yield x
          }
          .map(_._1)

      // if newH has been found , continue to find newC, then set confirmed, or nothing to do
      // find from the rest (drop high and higher)
      def newCommitBallotOptSP(high: Ballot): SP[F, Option[Ballot]] = {
        val rest = candidates.dropWhile(b => b >= high)
        // h should > p and p'
        val p = tracker.commitBallotIsNull &&
          (tracker.preparedBallotIsNull || !high.lessAndIncompatible(tracker.prepared)) &&
          (tracker.preparedPrimeBallotIsNull || !high.lessAndIncompatible(tracker.preparedPrime))

        ifM[Option[Ballot]](Option.empty[Ballot], _ => p) { _ =>
          rest
            .foldRight((Option.empty[Ballot], NO_BREAK).pureSP[F]) { (n, acc) =>
              val isBreak = n < tracker.current.ifNullThen(Ballot.Null)
              for {
                pre <- acc
                x <- ifM[(Option[Ballot], Boolean)](pre, !_._2 && !isBreak) { _ =>
                  for {
                    ratified <- federatedRatify(quorumSet,
                                                tracker.latestBallotEnvelope,
                                                acceptedPredict(n))
                  } yield if (ratified) (Option(n), BREAK) else pre
                }
              } yield x
            }
            .map(_._1)
        }
      }

      for {
        newHOpt <- newHightBallotOptSP
        trackerD0 <- ifM[Delta[BallotTracker]](Delta.unchanged(tracker), _ => newHOpt.isDefined) {
          _ =>
            for {
              newCOpt <- newCommitBallotOptSP(newHOpt.get)
              trackerD00 <- ballotService.setPreparedCommitted(tracker,
                                                               newCOpt.getOrElse(Ballot.Null),
                                                               newHOpt.get)
            } yield trackerD00
        }
        trackerD1 <- ifM[Delta[BallotTracker]](trackerD0, _.changed) { _ =>
          for {
            trackerD10 <- updateCurrentIfNeeded(trackerD0.data)
            trackerD11 <- emitCurrentStateStatement(trackerD10.data, quorumSet)
          } yield trackerD11
        }

      } yield trackerD1
    }
  }

  private def updateCurrentIfNeeded(tracker: BallotTracker): SP[F, Delta[BallotTracker]] = {
    if (tracker.currentBallotIsNull || tracker.current < tracker.high) {
      bumpToBallot(tracker, tracker.high, check = true)
    } else Delta.unchanged(tracker).pureSP[F]
  }
  private def bumpToBallot(tracker: BallotTracker,
                           ballot: Ballot,
                           check: Boolean): SP[F, Delta[BallotTracker]] = {
    require(tracker.currentBallotIsNull || ballot >= tracker.current)
    val bumped = tracker.currentBallotIsNull || tracker.current.counter != ballot.counter
    val t1     = tracker.copy(current = ballot)
    (if (bumped) Delta.changed(t1.copy(heardFromQuorum = true))
     else Delta.changed(t1)).pureSP[F]

  }

  private def getCommitBoundariesFromStatements(tracker: BallotTracker,
                                                ballot: Ballot): Set[Int] = {
    tracker.latestBallotEnvelope.foldLeft(Set.empty[Int]) {
      case (acc, (_, envelope)) =>
        envelope.statement.message match {
          case x: Message.Prepare =>
            if (ballot.compatible(x.ballot) && x.cCounter > 0) acc + x.cCounter + x.hCounter
            else acc
          case x: Message.Commit =>
            if (ballot.compatible(x.ballot)) acc + x.cCounter + x.hCounter else acc
          case x: Message.Externalize =>
            if (ballot.compatible(x.commit)) acc + x.commit.counter + x.hCounter + Int.MaxValue
            else acc
        }
    }
  }

  private def findExtendedInterval(tracker: BallotTracker,
                                   quorumSet: QuorumSet,
                                   boundaries: Set[Int],
                                   predicate: SPPredicate[F, Interval]): SP[F, Interval] = {
    // sorted
    val sorted   = boundaries.toVector.sorted
    val BREAK    = true
    val NO_BREAK = false

    sorted
      .foldRight((Interval(0, 0), NO_BREAK).pureSP[F]) { (n, acc) =>
        for {
          pre <- acc
          x <- ifM[(Interval, Boolean)](pre, !_._2) { _ =>
            val cur = if (pre._1.bothZero) pre._1.setBoth(n) else pre._1.shiftLeft(n)
            for {
              b <- predicate(cur)
            } yield if (b) (cur, NO_BREAK) else (pre._1, BREAK)
          }
        } yield x
      }
      .map(_._1)
  }

  private def setCommitAccept(tracker: BallotTracker,
                              candidate: Interval,
                              ballot: Ballot): SP[F, Delta[BallotTracker]] = {
    ifM[Delta[BallotTracker]](
      Delta.unchanged(tracker),
      _ =>
        candidate.first > 0 && (tracker.notCommitPhase || tracker.high.counter < candidate.second)) {
      x =>
        val c = Ballot(candidate.first, ballot.value)
        val h = Ballot(candidate.second, ballot.value)

        val shouldSetCommitAndHigh = tracker.highBallotIsNull || tracker.commitBallotIsNull ||
          tracker.high != h || tracker.commit != c

        val shouldSetPhaseAndPreparedPrime = tracker.isPreparePhase

        for {
          trackerD0 <- ifM[Delta[BallotTracker]](x, _ => shouldSetCommitAndHigh) { _ =>
            Delta.changed(tracker.copy(commit = c, high = h)).pureSP[F]
          }
          trackerD1 <- ifM[Delta[BallotTracker]](trackerD0, _ => shouldSetPhaseAndPreparedPrime) {
            _ =>
              val shouldBump = trackerD0.data.currentBallotNotNull && !h.lessAndCompatible(
                trackerD0.data.current)
              for {
                trackerD10 <- ifM[Delta[BallotTracker]](trackerD0, _ => shouldBump) { _ =>
                  bumpToBallot(tracker, h, check = false)
                }
                trackerD11 <- Delta
                  .changed(trackerD10.data.copy(phase = Phase.Commit, preparedPrime = Ballot.Null))
                  .pureSP[F]
              } yield trackerD11
          }
        } yield trackerD1
    }
  }

  private def setCommitConfirm(tracker: BallotTracker,
                               quorumSet: QuorumSet,
                               candidate: Interval,
                               ballot: Ballot): SP[F, Delta[BallotTracker]] = {
    ifM[Delta[BallotTracker]](tracker.unchanged, _ => candidate.first > 0) { _ =>
      val c              = Ballot(candidate.first, ballot.value)
      val h              = Ballot(candidate.second, ballot.value)
      val trackerUpdated = tracker.copy(commit = c, high = h)
      for {
        trackerD0 <- updateCurrentIfNeeded(trackerUpdated)
        trackerD1 <- emitCurrentStateStatement(trackerD0.data.copy(phase = Phase.Externalize),
                                               quorumSet)
        nominateTracker <- nodeStore.getNominateTracker(tracker.nodeID, tracker.slotIndex)
        _               <- nominateService.stopNomination(nominateTracker)
        _               <- nodeStore.saveNominateTracker(tracker.nodeID, nominateTracker)
        _ <- ballotService.externalizedValue(trackerD1.data.nodeID,
                                             trackerD1.data.slotIndex,
                                             trackerD1.data.commit.value)
      } yield trackerD1
    }
  }

  private def attemptCommitAccept[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    // check if in prepare or commit phase
    // check the ballot to commit is not null
    // if current phase is commit, ballot should compatible to high
    // check boundaries should not be null
    val ballot     = getAcceptCommitBallots(tracker, hint)
    val comp       = if (tracker.isCommitPhase) ballot.compatible(tracker.high) else true
    val boundaries = getCommitBoundariesFromStatements(tracker, ballot)
    def pred(interval: Interval): SP[F, Boolean] = {
      val votedPredict: Predicate[Statement[BallotMessage]] = { x =>
        x.message match {
          case y: Message.Prepare =>
            ballot.compatible(y.ballot) &&
              y.cCounter != 0 &&
              (y.cCounter <= interval.first && interval.second <= y.hCounter)
          case y: Message.Commit =>
            ballot.compatible(y.ballot) && y.cCounter <= interval.first
          case y: Message.Externalize =>
            ballot.compatible(y.commit) && y.commit.counter <= interval.first
        }
      }
      val acceptedPredict: Predicate[Statement[BallotMessage]] = { x =>
        x.message match {
          case _: Message.Prepare => false
          case y: Message.Commit =>
            ballot.compatible(y.ballot) &&
              (y.cCounter <= interval.first && interval.second <= y.hCounter)
          case y: Message.Externalize =>
            ballot.compatible(y.commit) && y.commit.counter <= interval.first
        }
      }

      federatedAccept(quorumSet, tracker.latestBallotEnvelope, votedPredict, acceptedPredict)
    }

    ifM[Delta[BallotTracker]](
      Delta.unchanged(tracker),
      _ => tracker.notExternalizePhase && ballot.notNull && comp && boundaries.nonEmpty) { _ =>
      for {
        interval  <- findExtendedInterval(tracker, quorumSet, boundaries, pred)
        trackerD0 <- setCommitAccept(tracker, interval, ballot)
        trackerD1 <- updateCurrentIfNeeded(trackerD0.data)
        trackerD2 <- emitCurrentStateStatement(trackerD1.data, quorumSet)
      } yield trackerD2
    }
  }

  private def getCommitConfirmBallot[M <: BallotMessage](hint: BallotStatement[M]): Ballot = {
    hint.message match {
      case _: Message.Prepare     => Ballot.Null // should be ignored
      case x: Message.Commit      => Ballot(x.hCounter, x.ballot.value)
      case x: Message.Externalize => Ballot(x.hCounter, x.commit.value)
    }
  }

  private def attemptCommitConfirm[M <: BallotMessage](
      tracker: BallotTracker,
      quorumSet: QuorumSet,
      hint: BallotStatement[M]): SP[F, Delta[BallotTracker]] = {
    // should be commit phase
    val shouldBeCommit = tracker.phase.isCommit
    // high and commit should not be null
    val highAndCommitShouldNotBeNull = tracker.highBallotNotNull && tracker.commitBallotNotNull
    // hint should not be prepare msg
    val ballot                 = getCommitConfirmBallot(hint)
    val ballotShouldNotBeNull  = ballot.notNull
    val ballotCompatibleCommit = ballot.compatible(tracker.commit)
    val boundaries             = getCommitBoundariesFromStatements(tracker, ballot)
    val pred: SPPredicate[F, Interval] = { check =>
      val commitPredict: Predicate[Statement[BallotMessage]] = { x =>
        x.message match {
          case _: Message.Prepare => false
          case y: Message.Commit =>
            ballot.compatible(y.ballot) &&
              (y.cCounter <= check.first && check.second <= y.hCounter)
          case y: Message.Externalize =>
            ballot.compatible(y.commit) && y.commit.counter <= check.first
        }
        true
      }
      federatedRatify(quorumSet, tracker.latestBallotEnvelope, commitPredict)
    }

    ifM[Delta[BallotTracker]](
      tracker.unchanged,
      _ =>
        shouldBeCommit && highAndCommitShouldNotBeNull &&
          ballotShouldNotBeNull && ballotCompatibleCommit && boundaries.nonEmpty) { _ =>
      for {
        interval  <- findExtendedInterval(tracker, quorumSet, boundaries, pred)
        trackerD0 <- setCommitConfirm(tracker, quorumSet, interval, ballot)
      } yield trackerD0
    }
  }

  private def attempBump(tracker: BallotTracker, quorumSet: QuorumSet): SP[F, Unit] = {
    if (tracker.isExternalizePhase) ().pureSP[F]
    else {
      def isVBlocking(counter: Int): Boolean = {
        val nodeIDs = tracker.latestBallotEnvelope
          .filter {
            case (nodeID, ballotEvelope) =>
              ballotEvelope.statement.message match {
                case x: Message.Prepare     => counter < x.ballot.counter
                case x: Message.Commit      => counter < x.ballot.counter
                case x: Message.Externalize => counter < Int.MaxValue
              }
          }
          .keys
          .toSet
        quorumSet.isQuorumSlice(nodeIDs)
      }

      // find all counters to a set
      val targetCounter = tracker.current.counter

      lazy val allCounters = tracker.latestBallotEnvelope
        .foldLeft(Set.empty[Int]) { (acc, n) =>
          n._2.statement.message match {
            case x: Message.Prepare     => acc + x.prepared.counter
            case x: Message.Commit      => acc + x.ballot.counter
            case x: Message.Externalize => acc + Int.MaxValue
          }
        }
        .toVector
        .filter(_ > targetCounter)
        .sorted

      if (isVBlocking(targetCounter)) {
        val smallestNotVBlocking = allCounters.find(x => !isVBlocking(x))
        if (smallestNotVBlocking.isDefined)
          for {
            _ <- nodeStore.saveBallotTracker(tracker.nodeID, tracker)
            _ <- abandonBallot(tracker.nodeID, tracker.slotIndex, smallestNotVBlocking.get)
          } yield ()
        else ().pureSP[F]
      } else ().pureSP[F]
    }
  }

  /**
    * advance the slot
    * @param statement statement
    * @param tracker tracker
    * @return
    */
  def advanceSlot[M <: BallotMessage](tracker: BallotTracker,
                                      quorumSet: QuorumSet,
                                      statement: BallotStatement[M]): SP[F, BallotTracker] = {
    //todo: attempt to bump state
    //todo: send latest envelope(if not sent)
    for {
      pa           <- attemptPreparedAccept(tracker, quorumSet, statement)
      pc           <- attemptPreparedConfirm(pa.data, quorumSet, statement)
      ca           <- attemptCommitAccept(pc.data, quorumSet, statement)
      cc           <- attemptCommitConfirm(ca.data, quorumSet, statement)
      _            <- attempBump(tracker, quorumSet)
      trackerFinal <- checkHeardFromQuorum(tracker)
    } yield trackerFinal.data
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
