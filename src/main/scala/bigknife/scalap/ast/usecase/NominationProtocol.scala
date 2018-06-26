package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._
import cats.implicits._

/**
  * nomination protocol of scp
  */
trait NominationProtocol[F[_]] extends BaseProtocol[F] {
  ballotProtocol: BallotProtocol[F] =>

  import model._

  /**
    * run nomination protocol
    * @param slot a slot
    * @param message nomination statement
    * @return
    */
  def runNominationProtocol(slot: Slot,
                            message: NominationMessage,
                            self: Boolean = false): SP[F, Result] = {

    val verify: SP[F, Boolean] = for {
      x <- isSane(message.statement)
      _ <- logService.info(s"is sane? $x for $message")
      y <- isNewer(slot, message.statement)
      _ <- logService.info(s"is newer? $x for $message")
    } yield x && y

    def votedPredict(value: Value) = Message.Statement.predict {
      case x: Message.Nominate => x.votes.contains(value)
      case _                   => false
    }

    def acceptedPredict(value: Value) = Message.Statement.predict {
      case x: Message.Nominate => x.accepted.contains(value)
      case _                   => false
    }

    // try to promote voted value to accepted
    def federatedAcceptNominations(slot: Slot): SP[F, Slot] = {
      message.statement match {
        case x: Message.Nominate =>
          x.votes.foldLeft(slot.pureSP[F]) { (acc, n) =>
            for {
              xSlot <- acc
              accepted <- federatedAccept(xSlot,
                                          votedPredict(n),
                                          acceptedPredict(n),
                                          xSlot.nominateTracker.latestNominations)
              ySlot <- if (!accepted) acc
              else
                for {
                  validity <- applicationExtension.validateNominationValue(n)
                  x0 <- {
                    validity match {
                      case Validity.FullyValidated =>
                        slotService.acceptNomination(xSlot, n): SP[F, Slot]
                      case _ => // try to transform value to value to vote
                        for {
                          transformed <- applicationExtension.extractValidValue(xSlot, n)
                          s0 <- if (transformed.isDefined)
                            slotService.voteNomination(xSlot, n): SP[F, Slot]
                          else acc
                        } yield s0
                    }
                  }
                } yield x0
            } yield ySlot
          }
      }
    }

    // try to promote accepted value to candidates
    def federatedRatifyNominations(slot: Slot): SP[F, Slot] = {
      message.statement match {
        case x: Message.Nominate =>
          x.accepted.foldLeft(slot.pureSP[F]) { (acc, n) =>
            if (slot.nominateTracker.candidates.contains(n)) acc
            else
              for {
                xSlot <- acc
                ratified <- federatedRatify(xSlot,
                                            acceptedPredict(n),
                                            xSlot.nominateTracker.latestNominations)
                ySlot <- if (ratified) slotService.candidateValue(xSlot, n): SP[F, Slot] else acc
              } yield ySlot
          }
      }
    }

    // if local node's candidates is empty, and the message sender is round leader
    // vote the nomination if the nomination is the largest to current votes and acceptes
    def takeRoundLeaderVotes(slot: Slot): SP[F, Slot] = {
      if (slot.nominateTracker.candidates.isEmpty &&
          slot.nominateTracker.roundLeaders.contains(message.statement.nodeId)) {
        message.statement match {
          case x: Message.Nominate =>
            val init = (Option.empty[Value], 0L).pureSP[F]
            val scanLargest: SP[F, (Option[Value], Long)] = (x.votes ++ x.accepted).foldLeft(init) {
              (acc, n) =>
                for {
                  pre      <- acc
                  validity <- applicationExtension.validateNominationValue(n)
                  valueToNominate <- {
                    validity match {
                      case Validity.FullyValidated => Option(n).pureSP[F]
                      case _ =>
                        for {
                          vOpt <- applicationExtension.extractValidValue(slot, n): SP[F, Option[Value]]
                        } yield vOpt
                    }
                  }
                  ret <- if (valueToNominate.isDefined && !slot.nominateTracker.voted.contains(
                               valueToNominate.get)) {
                    val n0 = for {
                      hash <- slotService.computeValueHash(slot, n)
                    } yield if (hash >= pre._2) (Option(n), hash) else pre
                    n0: SP[F, (Option[Value], Long)]
                  } else acc
                } yield ret
            }

            for {
              largest <- scanLargest
              xSlot <- if (largest._1.isDefined)
                slotService.voteNomination(slot, largest._1.get): SP[F, Slot]
              else slot.pureSP[F]
            } yield xSlot
        }
      } else
        slot.pureSP[F]
    }

    def gotNewCandidate(slot: Slot): SP[F, Slot] = {
      for {
        composite <- applicationExtension.combineValues(slot.nominateTracker.candidates)
        xSlot     <- slotService.updateCompositeCandidateValue(slot, composite)
        ySlot     <- ballotProtocol.bumpState(xSlot, composite, force = false)
      } yield ySlot
    }

    val process: SP[F, Result] = for {
      slot0        <- slotService.trackNewNominationMessage(slot, message)
      promotion1   <- federatedAcceptNominations(slot0)
      promotion2   <- federatedRatifyNominations(promotion1)
      promotion3   <- takeRoundLeaderVotes(promotion2)
      modified     <- slotService.hasBeenModifiedInNomination(slot0, promotion3)
      promotion4   <- if (modified) emitNomination(promotion3) else promotion3.pureSP[F]
      newCandidate <- slotService.hasNewCandidates(slot0, promotion4)
      promotion5   <- if (newCandidate) gotNewCandidate(promotion4) else promotion4.pureSP[F]
      r            <- validResult(promotion5)
    } yield r

    // process after verified
    for {
      passed <- verify
      result <- if (passed) process else invalidResult(slot)
    } yield result
  }

  def nominate(slot: Slot, value: Value, previousValue: Value, timeout: Boolean): SP[F, Boolean] = {
    def check(): SP[F, Boolean] =
      for {
        x <- if (timeout && !slot.nominateTracker.nominationStarted)
          for {
            _ <- logService.info("nominate TIMED OUT")
            x <- false.pureSP[F]
          } yield x
        else true.pureSP[F]
      } yield x

    def allQs(qs: QuorumSet, acc: Vector[QuorumSet]): Vector[QuorumSet] = {
      if (qs.innerSets.isEmpty) acc :+ qs
      else {
        val in = qs.innerSets.foldLeft(Vector.empty[QuorumSet]) { (acc1, n) =>
          acc1 ++ allQs(n, Vector.empty)
        }
        acc ++ in
      }
    }

    def getNewValueFromNomination(slot: Slot, nominationStatement: NominationStatement):SP[F, Option[Value]] = {
      nominationStatement match {
        case x: Message.Nominate =>
          (x.votes ++ x.accepted).foldLeft((Option.empty[Value], 0L).pureSP[F]) {(acc, n) =>
            for {
              pre <- acc
              vl <- applicationExtension.validateNominationValue(n)
              vlOpt <- if (vl == Value.Validity.FullyValidated) Option(n).pureSP[F] else
                applicationExtension.extractValidValue(slot, n): SP[F, Option[Value]]
              x <- if (vlOpt.isDefined && !slot.nominateTracker.voted.contains(vlOpt.get)) {
                for {
                  curHash <- slotService.computeValueHash(slot, vlOpt.get): SP[F, Long]
                } yield if(curHash >= pre._2) (vlOpt, curHash) else pre
              } else acc
            } yield x
          }.map(_._1)
      }
    }

    def getNominatingValue(slot: Slot): SP[F, Vector[Value]] = {
      if (slot.nominateTracker.roundLeaders.contains(slot.nodeId)) Vector(value).pureSP[F]
      else {
        val roundLeaders = slot.nominateTracker.roundLeaders.filter(slot.nominateTracker.latestNominations.contains)
        roundLeaders.foldLeft(Vector.empty[Value].pureSP[F]) { (acc, n) =>
          for {
            pre <- acc
            valueOpt <- getNewValueFromNomination(slot, slot.nominateTracker.latestNominations(n).statement)
          } yield if(valueOpt.isDefined) pre :+ valueOpt.get else pre
        }
      }
    }

    def findRoundLeaders(slot: Slot, qs: QuorumSet, topPriority: Long): SP[F, Vector[Node.ID]] = {

      val allFlatQs = allQs(qs, Vector.empty)

      // default roundLeaders include local node id
      allFlatQs
        .flatMap(_.validators)
        .foldLeft((Vector(slot.nodeId), Option.empty[Long]).pureSP[F]) { (acc, n) =>
          for {
            res         <- acc
            tp <- if (res._2.isEmpty) topPriority.pureSP[F] else res._2.get.pureSP[F]
            w           <- nodeService.getNodePriority(slot, n, qs): SP[F, Long]
            res <- if (w > tp) {
              (Vector.empty[Node.ID], Option(w)).pureSP[F]
            } else if (w == tp && w > 0) {
              (res._1 :+ n, res._2).pureSP[F]
            } else acc
          } yield res
        }
        .map(_._1)
    }

    def updateRoundLeaders(slot: Slot): SP[F, Slot] =
      for {
        qs           <- quorumSetService.quorumFunction(slot.nodeId)
        qsN          <- quorumSetService.normalizeQuorumSet(qs, slot.nodeId)
        topPriority  <- nodeService.getNodePriority(slot, slot.nodeId, qsN)
        roundLeaders <- findRoundLeaders(slot, qsN, topPriority)
        xSlot        <- slotService.setNominationRoundLeaders(slot, roundLeaders)
      } yield xSlot

    for {
      _ <- logService.info(
        s"nominate $value to Slot(${slot.index} at round ${slot.nominateTracker.roundNumber}")
      passed <- check()
      nv    <- getNominatingValue(slot)
      m <- if (passed && nv.nonEmpty) for {
        xSlot <- slotService.setNominatingValue(slot, nv, previousValue)
        ySlot <- updateRoundLeaders(xSlot)
        ts    <- applicationExtension.computeTimeoutForNomination(xSlot)
        _ <- applicationExtension.setupTimer(
          xSlot,
          ts,
          Callback(() => {nominate(xSlot, value, previousValue, timeout = true);()}))
        modified <- slotService.hasBeenModifiedInNomination(ySlot, slot)
        zSlot    <- if (modified) emitNomination(ySlot) else ySlot.pureSP[F]
        _        <- slotStore.saveSlotForNode(slot.nodeId, zSlot)
      } yield modified
      else passed.pureSP[F]
    } yield m
  }

  /**
    * is the statement sane?
    * @param statement statement
    * @return
    */
  private def isSane(statement: NominationStatement): SP[F, Boolean] =
    messageService.isSaneNominationStatement(statement)

  /**
    * is the coming message newer than the latest nomination message sent from the node saved in slot.
    * @param slot slot
    * @param statement coming message
    * @return
    */
  private def isNewer(slot: Slot, statement: NominationStatement): SP[F, Boolean] = {
    val savedNominationOpt = slot.nominateTracker.latestNominations.get(slot.nodeId)
    if (savedNominationOpt.isEmpty) true.pureP[F]
    else {
      messageService.firstNominationStatementIsNewer(statement, savedNominationOpt.get.statement)
    }
  }
  private def emitNomination(slot: Slot): SP[F, Slot] = {
    for {
      _      <- logService.info(s"try to emit nomination for $slot")
      qs     <- quorumSetService.quorumFunction(slot.nodeId)
      hash   <- quorumSetService.hashOfQuorumSet(qs)
      msg    <- messageService.createNominationMessage(slot, hash)
      _      <- logService.info(s"create new nomination message: $msg")
      result <- runNominationProtocol(slot, msg, self = true)
      _      <- logService.info(s"run nomination locally return $result")
      xSlot <- if (result._2 != Message.State.valid) result._1.pureSP[F]
      else {
        for {
          isNew <- if (slot.nominateTracker.lastEmittedMessage.isEmpty) true.pureSP[F]
          else
            messageService.firstNominationStatementIsNewer(
              msg.statement,
              slot.nominateTracker.lastEmittedMessage.get.statement): SP[F, Boolean]

          _ <- logService.debug(s"message try to be emitted isNew ? $isNew")

          s0 <- if (isNew) for {
            s1 <- slotService.emitNominateMessage(result._1, msg): SP[F, Slot]
            _ <- if (slot.fullValidated) applicationExtension.emitMessage(msg): SP[F, Unit]
            else ().pureSP[F]
          } yield s1
          else result._1.pureSP[F]
        } yield s0
      }
    } yield xSlot
  }
}
