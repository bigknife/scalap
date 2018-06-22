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
            _       <- if (didWork) sendLatestEnvelope(s6) else ().pureSP[F]
            r1      <- validResult(s6)
          } yield r1
      } yield r
    }

    // process for slot
    def processForSlot(slot: Slot): SP[F, Result] = {
      slot.ballotTracker.phase match {
        case Phase.Externalized =>
          for {
            workingBallot <- messageService.getWorkingBallot(message.statement)
            res <- if (slot.ballotTracker.commit.isDefined && slot.ballotTracker.commit.get.value == workingBallot.value)
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
  final def bumpState(slot: Slot, candidate: Value): SP[F, Unit] = {
    for {
      _ <- logService.info(s"//TODO: bump state: $candidate")
    } yield ()
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
        val ballotValue: Option[Value] =
          x.ballot.flatMap(x => if (x.counter != 0) Some(x.value) else None)
        val preparedValue: Option[Value] = x.prepared.map(_.value)
        ballotValue.toVector ++ preparedValue.toVector
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
          _        <- if (advanced) emitCurrentStatement(xSlot) else ().pureSP[F]
        } yield xSlot

    }
  }

  private def findPreparedAccepted(slot: Slot, ballots: Vector[Ballot]): SP[F, Option[Ballot]] = {
    ballots.sorted.foldRight(Option.empty[Ballot].pureSP[F]) { (n, acc) =>
      // passed conditions

      // only consider the ballot if it may help us increase
      // p (note: at this point, p ~ c)
      val cond1 = slot.ballotTracker.phase == Phase.Confirm && {
        (!(slot.ballotTracker.prepared.isDefined &&
          slot.ballotTracker.prepared.get <= n && slot.ballotTracker.prepared.get
          .compatible(n))) || { require(slot.ballotTracker.commit.get.compatible(n)); false }
      }

      // if we already prepared this ballot, don't bother checking again

      // if ballot <= p' ballot is neither a candidate for p nor p'
      val cond2 = slot.ballotTracker.preparedPrime.isDefined && (n <= slot.ballotTracker.preparedPrime.get)

      // if ballot is already covered by p, skip
      val cond3 = slot.ballotTracker.prepared.isDefined && (n <= slot.ballotTracker.prepared.get && n
        .compatible(slot.ballotTracker.prepared.get))

      // predict
      val votedPredict: Message.Statement.Predict = Message.Statement.predict {
        case x: Message.Prepare     => n <= x.ballot.get && n.compatible(x.ballot.get)
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
              if (highBallot.isDefined && highBallot.get >= n) {
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
      if (commit.isEmpty &&
          (prepared.isEmpty || !(rstart <= prepared.get && rstart.compatible(prepared.get))) &&
          (preparedPrime.isEmpty || !(rstart <= prepared.get && rstart.compatible(prepared.get)))) {
        val sorted = ballots.sorted
        val rest   = sorted.dropRight(sorted.length - sorted.indexOf(rstart) - 1)

        // result, break
        rest
          .foldRight((Option.empty[Ballot], false).pureSP[F]) { (n, acc) =>
            for {
              pre <- acc
              res <- if (pre._2) acc
              else {
                if (current.isDefined && n < current.get) (pre._1, true).pureSP[F]
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

    if (slot.ballotTracker.phase != Phase.Prepare || slot.ballotTracker.prepared.isEmpty)
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
              _ <- emitCurrentStatement(x)
            } yield x
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
        if (x.nC != 0) Some(Ballot(x.nH, x.ballot.get.value)) else Option.empty[Ballot]
      case x: BallotConfirmStatement     => Some(Ballot(x.nH, x.ballot.get.value))
      case x: BallotExternalizeStatement => Some(Ballot(x.nH, x.commit.value))
    }
    val cond2 = ballotOpt.isEmpty

    val cond3 = slot.ballotTracker.phase match {
      case Phase.Confirm if ballotOpt.isDefined && slot.ballotTracker.highBallot.isDefined =>
        !ballotOpt.get.compatible(slot.ballotTracker.highBallot.get)
      case _ => true
    }

    val boundaries = getCommitBoundariesFromStatements(slot, ballotOpt.get)
    val cond4      = boundaries.isEmpty

    def votedPredict(interval: Interval): Message.Statement.Predict = Message.Statement.predict {
      case x: BallotPrepareStatement =>
        if (x.ballot.get.compatible(ballotOpt.get) && x.nC != 0)
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
                     (slot.ballotTracker.currentBallot.isDefined && !(h < slot.ballotTracker.currentBallot.get && h
                       .compatible(slot.ballotTracker.currentBallot.get)))) {
              bumpToBallot(slot, h, check = false)
            } else ().pureSP[F]
            s0 <- slotService.setAcceptCommit(slot, c, h): SP[F, Slot]
          } yield s0

        } else slot.pureSP[F]
        modified <- slotService.hasAdvancedBallotProcess(slot, xSlot)
        ySlot <- if (modified) for {
          s0 <- updateCurrentIfNeeded(xSlot)
          _  <- emitCurrentStatement(s0)
        } yield s0
        else xSlot.pureSP[F]
      } yield ySlot
  }

  private def attemptConfirmCommit(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    val cond1 = slot.ballotTracker.phase != Phase.Confirm
    val cond2 = slot.ballotTracker.highBallot.isEmpty || slot.ballotTracker.commit.isEmpty
    val ballotOpt: Option[Ballot] = hint match {
      case _: BallotPrepareStatement     => None
      case x: BallotConfirmStatement     => Some(Ballot(x.nH, x.ballot.get.value))
      case x: BallotExternalizeStatement => Some(Ballot(x.nH, x.commit.value))
    }
    val cond3 = ballotOpt.isEmpty
    val cond4 = !ballotOpt.get.compatible(slot.ballotTracker.commit.get)

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
          _  <- emitCurrentStatement(s0)
        } yield s0
        else xSlot.pureSP[F]
      } yield ySlot
    }
  }

  private def attemptBump(slot: Slot): SP[F, Slot]          = ???
  private def checkHeardFromQuorum(slot: Slot): SP[F, Slot] = ???
  private def sendLatestEnvelope(slot: Slot): SP[F, Unit]   = ???
  private def emitCurrentStatement(slot: Slot): SP[F, Unit] = ???

  private def bumpToBallot(slot: Slot, ballot: Ballot, check: Boolean): SP[F, Slot] = {
    val gotBumped = slot.ballotTracker.currentBallot.isEmpty || slot.ballotTracker.currentBallot.get.counter != ballot.counter
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
      (x.prepared.isDefined && (n <= x.prepared.get && n.compatible(x.prepared.get))) ||
        (x.preparedPrime.isDefined && (n <= x.preparedPrime.get && n.compatible(
          x.preparedPrime.get)))

    case x: Message.Confirm =>
      val p = Ballot(x.nPrepared, x.ballot.get.value)
      n <= p && n.compatible(p)

    case x: Message.Externalize =>
      n.compatible(x.commit)

    case _ => false // impossible
  }

  private def getCommitBoundariesFromStatements(slot: Slot, ballot: Ballot): Vector[Int] = {
    slot.ballotTracker.latestBallotMessages.foldLeft(Vector.empty[Int]) {
      case (acc, (nodeId, msg)) =>
        msg.statement match {
          case x: BallotPrepareStatement =>
            if (ballot.compatible(x.ballot.get) && x.nC > 0) {
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
    if (current.isEmpty || current.get < high.get) {
      bumpToBallot(slot, high.get, check = true)
    } else slot.pureSP[F]
  }
}
