package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * nomination protocol of scp
  */
trait NominationProtocol[F[_]] extends BaseProtocol[F] {
  import model._

  /**
    * run nomination protocol
    * @param slot a slot
    * @param message nomination statement
    * @return
    */
  def runNominationProtocol(slot: Slot, message: NominationMessage): SP[F, Result] = {
    import cats.implicits._
    val verify: SP[F, Boolean] =
      (isSane(message.statement), isNewer(slot, message.statement)).mapN(_ && _)

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
                  validity <- validateNominationValue(value = n)
                  x0 <- {
                    validity match {
                      case Validity.FullyValidated =>
                        slotService.acceptNomination(xSlot, n): SP[F, Slot]
                      case _ => // try to transform value to value to vote
                        for {
                          transformed <- extractValidValue(xSlot, n)
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
                  validity <- validateNominationValue(n)
                  valueToNominate <- {
                    validity match {
                      case Validity.FullyValidated => Option(n).pureSP[F]
                      case _ =>
                        for {
                          vOpt <- extractValidValue(slot, n)
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

    def emitNomination(slot: Slot): SP[F, Slot] = {
      for {
        msg    <- messageService.createNominationMessage(slot)
        result <- runNominationProtocol(slot, msg)
        xSlot <- if (result._2 != Message.State.valid) result._1.pureSP[F]
        else {
          for {
            isNew <- if (slot.nominateTracker.lastEmittedMessage.isEmpty) true.pureSP[F]
            else
              messageService.firstNominationStatementIsNewer(
                msg.statement,
                slot.nominateTracker.lastEmittedMessage.get.statement): SP[F, Boolean]
            s0 <- if (isNew) for {
              s1 <- slotService.emitNominateMessage(result._1, msg): SP[F, Slot]
              _  <- if (slot.fullValidated) emitMessage(msg): SP[F, Unit] else ().pureSP[F]
            } yield s1
            else result._1.pureSP[F]
          } yield s0
        }
      } yield xSlot
    }

    val process: SP[F, Result] = for {
      slot0      <- slotService.trackNewNominationMessage(slot, message)
      promotion1 <- federatedAcceptNominations(slot0)
      promotion2 <- federatedRatifyNominations(promotion1)
      promotion3 <- takeRoundLeaderVotes(promotion2)
      modified   <- slotService.hasBeenModifiedInNomination(slot0, promotion3)
      promotion4 <- if (modified) emitNomination(promotion3) else promotion3.pureSP[F]
      r          <- validResult(promotion4)
    } yield r

    // process after verified
    for {
      passed <- verify
      result <- if (passed) process else invalidResult(slot)
    } yield result
  }

  /**
    * is the statement sane?
    * @param statement statement
    * @return
    */
  private def isSane(statement: NominationStatement): P[F, Boolean] = ???

  /**
    * is the coming message newer than the latest nomination message sent from the node saved in slot.
    * @param slot slot
    * @param statement coming message
    * @return
    */
  private def isNewer(slot: Slot, statement: NominationStatement): P[F, Boolean] = ???
}
