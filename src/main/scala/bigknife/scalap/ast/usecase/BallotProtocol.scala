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
            s1      <- attempPreparedConfirmed(s0, statement)
            s2      <- attempAcceptCommit(s1, statement)
            s3      <- attempConfirmCommit(s2, statement)
            s4      <- attempBump(s3)
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
      val acceptedPredict: Message.Statement.Predict = Message.Statement.predict {
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

      if (cond1 || cond2 || cond3) acc
      else
        for {
          ballotOpt <- acc
          xOpt <- if (ballotOpt.isDefined) acc
          else
            for {
              accepted <- federatedAccept(slot,
                                          votedPredict,
                                          acceptedPredict,
                                          slot.ballotTracker.latestBallotMessages)
              found <- if (accepted) Option(n).pureSP[F] else Option.empty[Ballot].pureSP[F]
            } yield found
        } yield xOpt
    }
  }

  private def attempPreparedConfirmed(slot: Slot, hint: BallotStatement): SP[F, Slot] = {
    if (slot.ballotTracker.phase != Phase.Prepare || slot.ballotTracker.prepared.isEmpty) slot.pureSP[F]
    else {
      val candidatesSP: SP[F, Vector[Ballot]] = messageService.getPreparedCandidates(slot, hint)

    }
  }

  private def findAcceptedCandidates(slot: Slot, ballots: Vector[Ballot]): SP[F, Option[Ballot]] = ???

  private def attempAcceptCommit(slot: Slot, hint: BallotStatement): SP[F, Slot]      = ???
  private def attempConfirmCommit(slot: Slot, hint: BallotStatement): SP[F, Slot]     = ???
  private def attempBump(slot: Slot): SP[F, Slot]                                     = ???
  private def checkHeardFromQuorum(slot: Slot): SP[F, Slot]                           = ???
  private def sendLatestEnvelope(slot: Slot): SP[F, Unit]                             = ???
}
