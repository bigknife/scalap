package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types.BallotTracker.Phase
import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * ballot protocol of scp protocol
  */
trait BallotProtocol[F[_]] extends BaseProtocol[F] {
  import model._

  // (low, high) pair
  private case class Interval(first: Int, second: Int)

  final def runBallotProtocol(slot: Slot,
                              message: BallotMessage,
                              self: Boolean = false): SP[F, Result] = {

    // advance slot with statement
    def advanceSlot(slot: Slot, statement: BallotStatement): SP[F, Result] = {
      for {
        xSlotAdvanced <- slotService.tryAdvanceSlotBallotMessageLevel(slot)
        _ <- logService.info(s"advanceSlot, currentMessageLevel = ${slot.ballotTracker.currentMessageLevel}")
        r <- if (!xSlotAdvanced._2) for {
          _  <- logService.info("maximum number of transitions reached in advanceSlot")
          r0 <- validResult(slot)
        } yield r0
        else
          for {
            s0      <- attemptPreparedAccept(xSlotAdvanced._1, statement)
            s1      <- attemptPreparedConfirmed(s0, statement)
            s2      <- attemptAcceptCommit(s1, statement)
            s3      <- attemptConfirmCommit(s2, statement)
            s4      <- attemptBump(s3)
            s5      <- checkHeardFromQuorum(s4)
            s6      <- slotService.backSlotBallotMessageLevel(s5)
            didWork <- slotService.hasAdvancedBallotProcess(slot, s6)
            s7      <- if (didWork) sendLatestEnvelope(s6) else s6.pureSP[F]
            r1      <- validResult(s7)
          } yield r1
      } yield r
    }

    // process for slot
    def processForSlot(slot: Slot): SP[F, Result] = {
      slot.ballotTracker.phase match {
        case Phase.Externalized =>
          for {
            workingBallot <- messageService.getWorkingBallot(message.statement)
            res <- if (slot.ballotTracker.commit.value == workingBallot.value)
              for {
                s0 <- slotService.tractNewBallotMessage(slot, message)
                r  <- validResult(s0)
              } yield r
            else invalidResult(slot)
          } yield res

        case _ =>
          for {
            s0 <- slotService.tractNewBallotMessage(slot, message)
            r  <- advanceSlot(s0, message.statement)
          } yield r
      }
    }

    // check the quorum set if sane
    val verify: SP[F, Boolean] = for {
      sane  <- isSane(message.statement, self = false)
      _     <- logService.info(s"is sane? $sane for $message")
      newer <- isNewer(slot, message.statement)
      _     <- logService.info(s"is newer? $newer for $message")
    } yield sane && newer

    val process: SP[F, Result] = {
      for {
        valSlot <- validateValues(slot, message.statement)
        result <- if (valSlot._1 == Validity.Invalid) for {
          _ <- logService.error(
            s"invalid value from ${if (self) "self" else "others"}, skipping $message")
          x <- invalidResult(valSlot._2)
        } yield x
        else processForSlot(valSlot._2)
      } yield result
    }

    // process after verified
    for {
      passed <- verify
      result <- if (passed) process else invalidResult(slot)
    } yield result

  }

  /**
    * bump state
    * @param slot slot
    * @param candidate composite candidate value
    * @return
    */
  final def bumpState(slot: Slot, candidate: Value, force: Boolean): SP[F, Slot] = {
    val c = slot.ballotTracker.currentBallot
    if (!force && c != Ballot.NullBallot) slot.pureSP[F]
    else {
      val n = if (c == Ballot.NullBallot) 1 else c.counter + 1
      bumpState(slot, candidate, n)
    }
  }

  final def bumpState(slot: Slot, candidate: Value, n: Int): SP[F, Slot] = {
    val h = slot.ballotTracker.highBallot

    def updateCurrentValue(slot: Slot, ballot: Ballot): SP[F, Slot] = {
      slot.ballotTracker.phase match {
        case Phase.Externalized => slot.pureSP[F]
        case _ =>
          val current = slot.ballotTracker.currentBallot
          val commit  = slot.ballotTracker.commit
          // conditions to bumpToBallot
          val cond1 = current <= ballot && commit.incompatible(ballot)
          val cond2 = current < ballot

          for {
            xSlot <- if (cond1 || cond2) bumpToBallot(slot, ballot, check = true)
            else slot.pureSP[F]
            _ <- checkInvariant(xSlot)
          } yield xSlot
      }
    }

    slot.ballotTracker.phase match {
      case Phase.Externalized => slot.pureSP[F]
      case _ =>
        val newB = Ballot(n, if (h != Ballot.NullBallot) h.value else candidate)
        for {
          _        <- logService.info(s"bumpState for Slot[${slot.index}] with value: ${newB.value}")
          xSlot    <- updateCurrentValue(slot, newB)
          modified <- slotService.hasAdvancedBallotProcess(slot, xSlot)
          ySlot <- if (modified) for {
            s0  <- emitCurrentStatement(xSlot)
            s1 <- checkHeardFromQuorum(s0)
          } yield s1
          else xSlot.pureSP[F]
        } yield ySlot
    }
  }

  private def isSane(statement: BallotStatement, self: Boolean): SP[F, Boolean] = {
    for {
      quorumSetOpt <- getQuorumSetFromStatement(statement)
      result <- if (quorumSetOpt.isEmpty) for {
        _ <- logService.info(s"no quorum set found from statement($statement)"): SP[F, Unit]
      } yield false
      else
        for {
          qsSane <- quorumSetService.isQuorumSetSane(quorumSetOpt.get, extractChecks = false)
          _ <- logService.info(
            s"quorum set is sane? $qsSane, quorum set (${quorumSetOpt.get}) from statement($statement)")
          statementSane <- if (!qsSane) false.pureSP[F]
          else messageService.isSaneBallotStatement(statement, self): SP[F, Boolean]
        } yield statementSane
    } yield result
  }

  private def isNewer(slot: Slot, statement: BallotStatement): SP[F, Boolean] = {
    val saved = slot.ballotTracker.latestBallotMessages.get(statement.nodeId)
    if (saved.isEmpty) true.pureSP
    else {
      messageService.firstBallotStatementIsNewer(statement, saved.get.statement)
    }
  }

  private def validateValues(slot: Slot,
                             statement: BallotStatement): SP[F, (Value.Validity, Slot)] = {

    val values: Vector[Value] = statement match {
      case x: BallotPrepareStatement =>
        val ballots  = if (x.ballot.counter != 0) Vector(x.ballot.value) else Vector()
        val prepares = if (x.prepared.counter != 0) Vector(x.prepared.value) else Vector()
        ballots ++ prepares
      case x: BallotConfirmStatement =>
        x.ballot.map(_.value).toVector
      case x: BallotExternalizeStatement =>
        Vector(x.commit.value)
    }

    values.foldLeft((Validity.fullyValidated, slot).pureSP[F]) { (acc, n) =>
      for {
        tr  <- validateBallotValue(n)
        pre <- acc
      } yield {
        tr match {
          case Validity.FullyValidated => pre
          case Validity.Invalid        => (Validity.invalid, pre._2)
          case Validity.MaybeValid     => (Validity.maybeValid, pre._2.copy(fullValidated = false))
        }
      }
    }
  }

  private def attemptPreparedAccept(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    slot.ballotTracker.phase match {
      case Phase.Externalized => slot.pureSP[F]
      case _ =>
        val candidatesSP: SP[F, Vector[Ballot]] = messageService.getPreparedCandidates(slot, hint)
        for {
          ballots     <- candidatesSP
          acceptedOpt <- findPreparedAccepted(slot, ballots)
          xSlot <- if (acceptedOpt.isDefined)
            slotService.setPreparedBallot(slot, acceptedOpt.get): SP[F, Slot]
          else slot.pureSP[F]
          advanced <- slotService.hasAdvancedBallotProcess(slot, xSlot)
          ySlot        <- if (advanced) emitCurrentStatement(xSlot) else xSlot.pureSP[F]
        } yield ySlot

    }
  }

  private def findPreparedAccepted(slot: Slot, ballots: Vector[Ballot]): SP[F, Option[Ballot]] = {
    ballots.sorted.foldRight(Option.empty[Ballot].pureSP[F]) { (n, acc) =>
      // passed conditions

      // only consider the ballot if it may help us increase
      // p (note: at this point, p ~ c)
      val cond1 = slot.ballotTracker.phase == Phase.Confirm && {
        (!(slot.ballotTracker.prepared <= n && slot.ballotTracker.prepared.compatible(n))) || {
          require(slot.ballotTracker.commit.compatible(n)); false
        }
      }

      // if we already prepared this ballot, don't bother checking again

      // if ballot <= p' ballot is neither a candidate for p nor p'
      lazy val cond2 = n <= slot.ballotTracker.preparedPrime

      // if ballot is already covered by p, skip
      lazy val cond3 = (n <= slot.ballotTracker.prepared) && n.compatible(slot.ballotTracker.prepared)

      // predict
      val votedPredict: Message.Statement.Predict = Message.Statement.predict {
        case x: Message.Prepare     => n <= x.ballot && n.compatible(x.ballot)
        case x: Message.Confirm     => n.compatible(x.ballot.get)
        case x: Message.Externalize => n.compatible(x.commit)
        case _                      => false // impossible
      }
      //val acceptedPredict: Message.Statement.Predict =

      if (cond1 || cond2 || cond3) acc
      else
        for {
          ballotOpt <- acc
          xOpt <- if (ballotOpt.isDefined) acc
          else
            for {
              accepted <- federatedAccept(slot,
                                          votedPredict,
                                          acceptedPredict(n),
                                          slot.ballotTracker.latestBallotMessages)
              found <- if (accepted) Option(n).pureSP[F] else Option.empty[Ballot].pureSP[F]
            } yield found
        } yield xOpt
    }
  }

  private def attemptPreparedConfirmed(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    def findNewH(slot: Slot, ballots: Vector[Ballot]): SP[F, Option[Ballot]] = {
      // (result, break)
      val sorted = ballots.sorted
      val newHOpt: SP[F, (Option[Ballot], Boolean)] =
        sorted.foldRight((Option.empty[Ballot], false).pureSP[F]) { (n, acc) =>
          for {
            pre <- acc
            next <- if (pre._2) acc
            else {
              val highBallot = slot.ballotTracker.highBallot
              if (highBallot >= n) {
                (pre._1, true).pureSP[F]
              } else {
                for {
                  ratified <- federatedRatify(slot,
                                              acceptedPredict(n),
                                              slot.ballotTracker.latestBallotMessages)
                  b <- if (ratified) (Option(n), true).pureSP[F] else acc
                } yield b
              }
            }
          } yield next
        }
      newHOpt.map(_._1)
    }
    def findNewC(slot: Slot, ballots: Vector[Ballot], rstart: Ballot): SP[F, Option[Ballot]] = {
      val commit        = slot.ballotTracker.commit
      val prepared      = slot.ballotTracker.prepared
      val preparedPrime = slot.ballotTracker.preparedPrime
      val current       = slot.ballotTracker.currentBallot
      if (commit == Ballot.NullBallot &&
          (!(rstart <= prepared && rstart.compatible(prepared))) &&
          (!(rstart <= prepared && rstart.compatible(prepared)))) {
        val sorted = ballots.sorted
        val rest   = sorted.dropRight(sorted.length - sorted.indexOf(rstart) - 1)

        // result, break
        rest
          .foldRight((Option.empty[Ballot], false).pureSP[F]) { (n, acc) =>
            for {
              pre <- acc
              res <- if (pre._2) acc
              else {
                if (n < current) (pre._1, true).pureSP[F]
                else {
                  for {
                    ratified <- federatedRatify(slot,
                                                acceptedPredict(n),
                                                slot.ballotTracker.latestBallotMessages)
                  } yield if (ratified) (Option(n), false) else (pre._1, true)
                }
              }
            } yield res
          }
          .map(_._1)
      } else Option.empty[Ballot].pureSP[F]

    }

    if (slot.ballotTracker.phase != Phase.Prepare || slot.ballotTracker.prepared == Ballot.NullBallot)
      slot.pureSP[F]
    else {
      val candidatesSP: SP[F, Vector[Ballot]] = messageService.getPreparedCandidates(slot, hint)
      for {
        candidates <- candidatesSP
        newHOpt    <- findNewH(slot, candidates)
        xSlot <- if (newHOpt.isDefined) {
          for {
            newCOpt  <- findNewC(slot, candidates, newHOpt.get)
            s0       <- slotService.setPreparedConfirmed(slot, newCOpt, newHOpt)
            advanced <- slotService.hasAdvancedBallotProcess(slot, s0)
            s1 <- if (advanced) for {
              x <- updateCurrentIfNeeded(s0)
              y <- emitCurrentStatement(x)
            } yield y
            else s0.pureSP[F]
          } yield s1
        } else slot.pureSP[F]
      } yield xSlot
    }
  }

  private def attemptAcceptCommit(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    //// slot.ballotTracker.phase != Phase.Prepare && slot.ballotTracker.phase != Phase.Confirm
    val cond1 = slot.ballotTracker.phase == Phase.Externalized

    // extracts value from hint
    // note: ballot.counter is only used for logging purpose as we're looking at
    // possible value to commit
    val ballotOpt: Option[Ballot] = hint match {
      case x: BallotPrepareStatement =>
        if (x.nC != 0) Some(Ballot(x.nH, x.ballot.value)) else Option.empty[Ballot]
      case x: BallotConfirmStatement     => Some(Ballot(x.nH, x.ballot.get.value))
      case x: BallotExternalizeStatement => Some(Ballot(x.nH, x.commit.value))
    }
    lazy val cond2 = ballotOpt.isEmpty

    lazy val cond3 = slot.ballotTracker.phase match {
      case Phase.Confirm
          if ballotOpt.isDefined && slot.ballotTracker.highBallot != Ballot.NullBallot =>
        !ballotOpt.get.compatible(slot.ballotTracker.highBallot)
      case _ => true
    }

    lazy val boundaries = getCommitBoundariesFromStatements(slot, ballotOpt.get)
    lazy val cond4      = boundaries.isEmpty

    def votedPredict(interval: Interval): Message.Statement.Predict = Message.Statement.predict {
      case x: BallotPrepareStatement =>
        if (x.ballot.compatible(ballotOpt.get) && x.nC != 0)
          x.nC <= interval.first && interval.second <= x.nH
        else false
      case x: BallotConfirmStatement =>
        if (x.ballot.get.compatible(ballotOpt.get)) x.nCommit <= interval.first else false
      case x: BallotExternalizeStatement =>
        if (x.commit.compatible(ballotOpt.get)) x.commit.counter <= interval.first else false
      case _ => false // impossible
    }
    def acceptedPredict(interval: Interval): Message.Statement.Predict = Message.Statement.predict {
      case x: BallotStatement => commitPredict(ballotOpt.get, interval, x)
      case _                  => false // impossible
    }

    val predict: Interval => SP[F, Boolean] = { interval =>
      federatedAccept(slot,
                      votedPredict(interval),
                      acceptedPredict(interval),
                      slot.ballotTracker.latestBallotMessages)
    }

    // cond1/2/3/4 if true, exit
    if (cond1 || cond2 || cond3 || cond4) slot.pureSP[F]
    else
      for {
        candidate <- findExtendedInterval(boundaries)(predict)
        xSlot <- if (candidate.first != 0 && (slot.ballotTracker.phase != Phase.Confirm)) {
          val c = Ballot(candidate.first, ballotOpt.get.value)
          val h = Ballot(candidate.second, ballotOpt.get.value)
          for {
            _ <- if (slot.ballotTracker.phase == Phase.Prepare &&
                     (slot.ballotTracker.currentBallot != Ballot.NullBallot && !(h < slot.ballotTracker.currentBallot && h
                       .compatible(slot.ballotTracker.currentBallot)))) {
              bumpToBallot(slot, h, check = false)
            } else ().pureSP[F]
            s0 <- slotService.setAcceptCommit(slot, c, h): SP[F, Slot]
          } yield s0

        } else slot.pureSP[F]
        modified <- slotService.hasAdvancedBallotProcess(slot, xSlot)
        ySlot <- if (modified) for {
          s0 <- updateCurrentIfNeeded(xSlot)
          s1  <- emitCurrentStatement(s0)
        } yield s1
        else xSlot.pureSP[F]
      } yield ySlot
  }

  private def attemptConfirmCommit(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    val cond1      = slot.ballotTracker.phase != Phase.Confirm
    lazy val cond2 = slot.ballotTracker.highBallot == Ballot.NullBallot || slot.ballotTracker.commit == Ballot.NullBallot
    val ballotOpt: Option[Ballot] = hint match {
      case _: BallotPrepareStatement     => None
      case x: BallotConfirmStatement     => Some(Ballot(x.nH, x.ballot.get.value))
      case x: BallotExternalizeStatement => Some(Ballot(x.nH, x.commit.value))
    }
    lazy val cond3 = ballotOpt.isEmpty
    lazy val cond4 = !ballotOpt.get.compatible(slot.ballotTracker.commit)

    if (cond1 || cond2 || cond3 || cond4) slot.pureSP[F]
    else {
      def acceptedPredict(interval: Interval): Message.Statement.Predict =
        Message.Statement.predict {
          case x: BallotStatement => commitPredict(ballotOpt.get, interval, x)
          case _                  => false // impossible
        }
      val predict: Interval => SP[F, Boolean] = { interval =>
        federatedRatify(slot, acceptedPredict(interval), slot.ballotTracker.latestBallotMessages)
      }

      val boundaries = getCommitBoundariesFromStatements(slot, ballotOpt.get)
      for {
        candidate <- findExtendedInterval(boundaries)(predict)
        xSlot <- if (candidate.first != 0) {
          val c = Ballot(candidate.first, ballotOpt.get.value)
          val h = Ballot(candidate.second, ballotOpt.get.value)
          slotService.setConfirmCommit(slot, c, h): SP[F, Slot]
        } else slot.pureSP[F]
        modified <- slotService.hasAdvancedBallotProcess(slot, xSlot)
        ySlot <- if (modified) for {
          s0 <- updateCurrentIfNeeded(xSlot)
          s1 <- emitCurrentStatement(s0)
        } yield s1
        else xSlot.pureSP[F]
      } yield ySlot
    }
  }

  private def attemptBump(slot: Slot): SP[F, Slot] = {
    slot.ballotTracker.phase match {
      case Phase.Externalized => slot.pureSP[F]
      case _ =>
        val currentBallot = slot.ballotTracker.currentBallot
        val latestMsgs    = slot.ballotTracker.latestBallotMessages

        val targetCounter = currentBallot.counter

        val allCounters = latestMsgs
          .foldLeft(Set(targetCounter)) {
            case (acc, (_, msg)) =>
              msg.statement match {
                case x: BallotPrepareStatement     => acc + x.ballot.counter
                case x: BallotConfirmStatement     => acc + x.ballot.get.counter
                case _: BallotExternalizeStatement => acc + Int.MaxValue
              }
          }
          .toVector
          .sorted

        // go through the counters, find the smallest not v-blocking
        def filterNodeIds(slot: Slot, n: Int): Vector[Node.ID] =
          slot.ballotTracker.latestBallotMessages
            .filter {
              case (_, msg) =>
                msg.statement match {
                  case x: BallotPrepareStatement =>
                    n < x.ballot.counter
                  case x: BallotConfirmStatement =>
                    n < x.ballot.get.counter
                  case _ => n != Int.MaxValue
                }
            }
            .keys
            .toVector

        def findSmallestNotVBlocking(slot: Slot, counters: Vector[Int]): SP[F, Option[Int]] = {
          counters
            .foldLeft((Option.empty[Int], false).pureSP[F]) { (acc, n) =>
              for {
                pre <- acc
                x <- if (pre._2) acc
                else
                  for {
                    qs <- quorumSetService.quorumFunction(slot.nodeId): SP[F, QuorumSet]
                    vBlocking <- quorumSetService.isVBlocking(qs, filterNodeIds(slot, n)): SP[
                      F,
                      Boolean]
                    x0 <- if (n == targetCounter && !vBlocking) (pre._1, true).pureSP[F]
                    else {
                      if (!vBlocking) (Option(n), true).pureSP[F] else acc
                    }
                  } yield x0
              } yield x
            }
            .map(_._1)
        }

        for {
          iOpt  <- findSmallestNotVBlocking(slot, allCounters)
          xSlot <- if (iOpt.isEmpty) slot.pureSP[F] else abandonBallot(slot, iOpt.get)
        } yield xSlot
    }
  }
  private def abandonBallot(slot: Slot, n: Int): SP[F, Slot] = {
    val v = slot.nominateTracker.latestCompositeCandidate
      .getOrElse(slot.ballotTracker.currentBallot.value)
    if (v == Ballot.NullBallot) slot.pureSP[F]
    else {
      if (n == 0) bumpState(slot, v, force = true)
      else bumpState(slot, v, n)
    }
  }

  private def checkHeardFromQuorum(slot: Slot): SP[F, Slot] = {
    val current = slot.ballotTracker.currentBallot
    val filter: Message.Statement => Boolean = {
      case x: BallotPrepareStatement =>
        slot.ballotTracker.currentBallot <= x.ballot
      case _ => true
    }
    if (current != Ballot.NullBallot) {
      for {
        qs <- quorumSetService.quorumFunction(slot.nodeId)
        isQ <- {
          // isQuorum
          val filteredNodes = slot.ballotTracker.latestBallotMessages
            .filter(x => filter(x._2.statement))
            .keys
            .toVector
          val qsNodesSP: SP[F, Vector[Node.ID]] = filteredNodes.foldLeft(filteredNodes.pureSP[F]) {
            (acc, n) =>
              for {
                pre <- acc
                qsOpt <- getQuorumSetFromStatement(
                  slot.ballotTracker.latestBallotMessages(n).statement)
                x <- if (qsOpt.isEmpty) pre.filter(_ != n).pureSP[F]
                else
                  for {
                    isQs <- quorumSetService.isQuorumSlice(qsOpt.get, pre)
                    x0   <- (if (isQs) pre else pre.filter(_ != n)).pureSP[F]
                  } yield x0
              } yield x
          }

          for {
            qsNodes <- qsNodesSP
            isQs    <- quorumSetService.isQuorumSlice(qs, qsNodes): SP[F, Boolean]
          } yield isQs
        }

        xSlot <- if (isQ) {
          slot.ballotTracker.phase match {
            case Phase.Externalized =>
              for {
                _ <- applicationExtension.stopBallotProtocolTimer(slot): SP[F, Unit]
              } yield slot
            case _ =>
              val heardEver = slot.ballotTracker.heardFromQuorum
              for {
                _ <- applicationExtension.ballotDidHearFromQuorum(slot, current)
                s0 <- if (heardEver) slot.pureSP[F]
                else
                  for {
                    _  <- applicationExtension.startBallotProtocolTimer(slot): SP[F, Unit]
                    s1 <- slotService.setHeardFromQuorum(slot, heard = true): SP[F, Slot]
                  } yield s1
              } yield s0
          }
        } else slot.pureSP[F]
      } yield xSlot

    } else slot.pureSP[F]
  }
  private def sendLatestEnvelope(slot: Slot): SP[F, Slot] = {
    if (slot.ballotTracker.currentMessageLevel == 0 && slot.ballotTracker.lastMessage.isDefined && slot.fullValidated) {
      //println(slot.ballotTracker.currentMessageLevel)
      if (slot.ballotTracker.lastEmittedMessage.isEmpty || !slot.ballotTracker.lastMessage.contains(
            slot.ballotTracker.lastEmittedMessage.get)) {
        for {
          xSlot <- slotService.emitLatestBallotMessage(slot)
          _     <- applicationExtension.emitMessage(xSlot.ballotTracker.lastEmittedMessage.get)
        } yield xSlot
      } else slot.pureSP[F]
    } else slot.pureSP[F]
  }
  private def emitCurrentStatement(slot: Slot): SP[F, Slot] = {
    val lastMsg = slot.ballotTracker.latestBallotMessages.get(slot.nodeId)
    def shouldSend(msg: BallotMessage): SP[F, Boolean] =
      for {
        cond1 <- (slot.ballotTracker.currentBallot != Ballot.NullBallot).pureSP[F]
        cond2 <- if (slot.ballotTracker.lastMessage.isDefined)
          messageService.firstBallotStatementIsNewer(
            msg.statement,
            slot.ballotTracker.lastMessage.get.statement): SP[F, Boolean]
        else true.pureSP[F]
      } yield cond1 && cond2

    for {
      _      <- checkInvariant(slot)
      qs     <- quorumSetService.quorumFunction(slot.nodeId)
      qsHash <- quorumSetService.hashOfQuorumSet(qs)
      msg    <- messageService.createBallotMessage(slot, qsHash)
      xSlot <- if (lastMsg.isEmpty || lastMsg.get != msg) for {
        result <- runBallotProtocol(slot, msg, self = true)
        s0 <- if (result._2 == Message.State.Valid) for {
          send <- shouldSend(msg)
          s3 <- if (send) for {
            s1 <- slotService.emitBallotMessage(result._1, msg)
            s2 <- sendLatestEnvelope(s1)
          } yield s2
          else result._1.pureSP[F]
        } yield s3
        else result._1.pureSP[F]
      } yield s0
      else slot.pureSP[F]
    } yield xSlot
  }

  private def checkInvariant(slot: Slot): SP[F, Unit] = {
    // only log(error) now
    val phase        = slot.ballotTracker.phase
    val current      = slot.ballotTracker.currentBallot
    val prepare      = slot.ballotTracker.prepared
    val preparePrime = slot.ballotTracker.preparedPrime
    val commit       = slot.ballotTracker.commit
    val high         = slot.ballotTracker.highBallot
    def areBallotsLessAndIncompatible(b1: Ballot, b2: Ballot): Boolean =
      b1 <= b2 && !b1.compatible(b2)
    for {
      _ <- if (phase == Phase.Confirm && commit.isZero)
        logService.error("when Phase.Confirm, commit should not be empty"): SP[F, Unit]
      else ().pureSP[F]
      _ <- if (phase == Phase.Externalized && (commit.isZero || high.isZero))
        logService.error("when Phase.Confirm, commit and high should not be empty"): SP[F, Unit]
      else ().pureSP[F]
      _ <- if (current.counter <= 0)
        logService.error("current ballot's counter should <= 0"): SP[F, Unit]
      else ().pureSP[F]
      _ <- if (prepare.isNotZero && preparePrime.isNotZero && !areBallotsLessAndIncompatible(preparePrime, prepare))
        logService.error("preparePrime should less than and incompatible to prepare"): SP[F, Unit]
      else ().pureSP[F]
      _ <- if (commit.isNotZero && current.isZero)
        logService.error("when commit is defined, current ballot should be empty"): SP[F, Unit]
      else ().pureSP[F]
      _ <- if (commit.isNotZero && !areBallotsLessAndIncompatible(commit, high))
        logService.error("when commit is defined, commit should less than and incompatible to high"): SP[
          F,
          Unit]
      else ().pureSP[F]
      _ <- if (commit.isNotZero && !areBallotsLessAndIncompatible(high, current))
        logService.error(
          "when commit is defined, high should less than and incompatible to current"): SP[F, Unit]
      else ().pureSP[F]

    } yield ()
  }

  private def bumpToBallot(slot: Slot, ballot: Ballot, check: Boolean): SP[F, Slot] = {
    val gotBumped = slot.ballotTracker.currentBallot.isZero || slot.ballotTracker.currentBallot.counter != ballot.counter
    for {
      _ <- logService.info(s"slot(${slot.index}) bumpToBallot ($ballot)")
      xSlot <- if (slot.ballotTracker.phase == Phase.Externalized) for {
        _  <- logService.error("can't bumpToBallot when Phase.Externalized")
        s0 <- slot.pureSP[F]
      } yield s0
      else slotService.setBumpBallot(slot, ballot, gotBumped): SP[F, Slot]
    } yield xSlot
  }

  private def acceptedPredict(n: Ballot): Message.Statement.Predict = Message.Statement.predict {
    case x: Message.Prepare =>
      (x.prepared != Ballot.NullBallot && (n <= x.prepared && n.compatible(x.prepared))) ||
        (x.preparedPrime != Ballot.NullBallot && (n <= x.preparedPrime && n.compatible(
          x.preparedPrime)))

    case x: Message.Confirm =>
      val p = Ballot(x.nPrepared, x.ballot.get.value)
      n <= p && n.compatible(p)

    case x: Message.Externalize =>
      n.compatible(x.commit)

    case _ => false // impossible
  }

  private def getCommitBoundariesFromStatements(slot: Slot, ballot: Ballot): Vector[Int] = {
    slot.ballotTracker.latestBallotMessages.foldLeft(Vector.empty[Int]) {
      case (acc, (_, msg)) =>
        msg.statement match {
          case x: BallotPrepareStatement =>
            if (ballot.compatible(x.ballot) && x.nC > 0) {
              val xs = (if (!acc.contains(x.nC)) Vector(x.nC) else Vector()) ++
                (if (!acc.contains(x.nH)) Vector(x.nH) else Vector())
              acc ++ xs
            } else acc
          case x: BallotConfirmStatement =>
            if (ballot.compatible(x.ballot.get)) {
              val xs = (if (!acc.contains(x.nCommit)) Vector(x.nCommit) else Vector()) ++
                (if (!acc.contains(x.nH)) Vector(x.nH) else Vector())
              acc ++ xs
            } else acc
          case x: BallotExternalizeStatement =>
            if (ballot.compatible(x.commit)) {
              val xs = (if (!acc.contains(x.commit.counter)) Vector(x.commit.counter) else Vector()) ++
                (if (!acc.contains(x.nH)) Vector(x.nH) else Vector()) ++
                (if (!acc.contains(Int.MaxValue)) Vector(Int.MaxValue) else Vector())
              acc ++ xs
            } else acc
        }
    }
  }

  private def findExtendedInterval(boundaries: Vector[Int])(
      predict: Interval => SP[F, Boolean]): SP[F, Interval] = {
    // from right(top)
    boundaries.sorted
      .foldRight((Interval(0, 0), false).pureSP[F]) { (n, accAndBreakSP) =>
        for {
          accAndBreak <- accAndBreakSP
          next <- if (accAndBreak._2) accAndBreakSP
          else
            for {
              x <- (if (accAndBreak._1.first == 0) Interval(n, n)
                    else if (n > accAndBreak._1.second) accAndBreak._1
                    else Interval(n, accAndBreak._1.second)).pureSP[F]
              p <- predict(x)
            } yield if (p) (x, false) else (x, true)
        } yield next
      }
      .map(_._1)
  }

  private def commitPredict(ballot: Ballot,
                            check: Interval,
                            statement: BallotStatement): Boolean = {
    statement match {
      case _: BallotPrepareStatement => false
      case x: BallotConfirmStatement =>
        if (ballot.compatible(x.ballot.get)) x.nCommit <= check.first && check.second <= x.nH
        else false
      case x: BallotExternalizeStatement =>
        if (ballot.compatible(x.commit)) x.commit.counter <= check.first else false
    }
  }

  private def updateCurrentIfNeeded(slot: Slot): SP[F, Slot] = {
    val current = slot.ballotTracker.currentBallot
    val high    = slot.ballotTracker.highBallot
    if (current.isZero || current < high) {
      bumpToBallot(slot, high, check = true)
    } else slot.pureSP[F]
  }
}
