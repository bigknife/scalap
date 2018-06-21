package bigknife.scalap.interpreter
package service

import java.math.BigInteger

import bigknife.scalap.ast.service.SlotService
import bigknife.scalap.ast.types.Node.ID
import bigknife.scalap.ast.types._
import org.bouncycastle.jcajce.provider.digest.SHA3

class SlotServiceHandler extends SlotService.Handler[Stack] {
  import scala.collection._

  private val latestNominationStore: mutable.Map[(Node.ID, Long), NominationStatement] =
    mutable.Map.empty

  override def trackNewNominationMessage(slot: Slot,
                                         nominationMessage: NominationMessage): Stack[Slot] =
    Stack {
      slot.copy(
        nominateTracker = slot.nominateTracker.copy(
          latestNominations = slot.nominateTracker.latestNominations + (nominationMessage.statement.nodeId -> nominationMessage)),
        statementHistory = slot.statementHistory :+ Message.HistoricalStatement(
          nominationMessage.statement,
          System.currentTimeMillis(),
          slot.fullValidated)
      )
    }

  override def tractNewBallotMessage(slot: Slot, ballotMessage: BallotMessage): Stack[Slot] =
    Stack {
      slot.copy(
        ballotTracker = slot.ballotTracker.copy(
          latestBallotMessages = slot.ballotTracker.latestBallotMessages + (ballotMessage.statement.nodeId -> ballotMessage)),
        statementHistory = slot.statementHistory :+ Message.HistoricalStatement(
          ballotMessage.statement,
          System.currentTimeMillis(),
          slot.fullValidated)
      )
    }

  override def acceptNomination(slot: Slot, value: Value): Stack[Slot] = Stack {

    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        voted =
          if (slot.nominateTracker.voted.contains(value)) slot.nominateTracker.voted
          else slot.nominateTracker.voted :+ value,
        accepted =
          if (slot.nominateTracker.accepted.contains(value)) slot.nominateTracker.accepted
          else slot.nominateTracker.accepted :+ value
      )
    )
  }

  override def voteNomination(slot: Slot, value: Value): Stack[Slot] = Stack {
    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        voted =
          if (slot.nominateTracker.voted.contains(value)) slot.nominateTracker.voted
          else slot.nominateTracker.voted :+ value
      )
    )
  }

  override def candidateValue(slot: Slot, value: Value): Stack[Slot] = Stack {
    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        candidates = slot.nominateTracker.candidates :+ value
      )
    )
  }

  override def hasBeenModifiedInNomination(s1: Slot, s2: Slot): Stack[Boolean] = Stack {
    (s2.nominateTracker.voted != s1.nominateTracker.voted) ||
    (s2.nominateTracker.accepted != s1.nominateTracker.accepted)
  }

  override def emitNominateMessage(slot: Slot, message: NominationMessage): Stack[Slot] = Stack {
    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        lastEmittedMessage = Option(message)
      )
    )
  }

  override def createSlot(nodeId: ID, slotIndex: Long): Stack[Slot] = Stack {
    Slot(
      nodeId,
      0,
      NominateTracker.Empty,
      BallotTracker.Empty,
      Vector.empty,
      fullValidated = true
    )
  }

  override def latestNominationStatement(
      slot: Slot,
      default: Message.NominationStatement): Stack[Message.NominationStatement] =
    Stack {
      latestNominationStore.getOrElse((slot.nodeId, slot.index), default)
    }

  override def computeValueHash(slot: Slot, value: Value): Stack[Long] =
    Stack {
      val hash = new SHA3.Digest256().digest(value.asBytes ++ s"${slot.index}".getBytes())
      new BigInteger(hash).longValue()
    }

  override def hasNewCandidates(s1: Slot, s2: Slot): Stack[Boolean] = Stack {
    s2.index == s1.index &&
    s2.nominateTracker.candidates.length > s1.nominateTracker.candidates.length &&
    s2.nominateTracker.candidates.startsWith(s1.nominateTracker.candidates)
  }

  override def updateCompositeCandidateValue(slot: Slot, compositeValue: Value): Stack[Slot] =
    Stack {
      slot.copy(
        nominateTracker = slot.nominateTracker.copy(
          latestCompositedCandidate = Some(compositeValue)
        )
      )
    }

  override def setPreparedBallot(slot: Slot, ballot: Ballot): Stack[Slot] = Stack {
    val xSlot = slot.ballotTracker.prepared match {
      case Some(p) =>
        if (p < ballot) {
          val npp = if (!p.compatible(ballot)) Option(p) else slot.ballotTracker.preparedPrime
          val np  = Option(ballot)
          slot.copy(
            ballotTracker = slot.ballotTracker.copy(
              prepared = np,
              preparedPrime = npp
            ))
        } else if (p > ballot) {
          // check if we should update only p'
          if (slot.ballotTracker.preparedPrime.isEmpty || slot.ballotTracker.preparedPrime.get < ballot) {
            slot.copy(ballotTracker = slot.ballotTracker.copy(preparedPrime = Option(ballot)))
          } else slot
        } else slot
      case None => slot.copy(ballotTracker = slot.ballotTracker.copy(prepared = Some(ballot)))
    }

    //todo: setPreparedAccept see BallotProtocol.cpp#869

    xSlot
  }

  override def tryAdvanceSlotBallotMessageLevel(slot: Slot): Stack[(Slot, Boolean)] = Stack {
    setting =>
      if (slot.ballotTracker.currentMessageLevel + 1 >= setting.maxBallotMessageLevel) (slot, false)
      else
        (slot.copy(
           ballotTracker = slot.ballotTracker.copy(
             currentMessageLevel = slot.ballotTracker.currentMessageLevel + 1)),
         true)
  }

  override def backSlotBallotMessageLevel(slot: Slot): Stack[Slot] = Stack {
    slot.copy(
      ballotTracker = slot.ballotTracker.copy(
        currentMessageLevel = slot.ballotTracker.currentMessageLevel - 1
      )
    )
  }

  override def hasAdvancedBallotProcess(s1: Slot, s2: Slot): Stack[Boolean] = Stack {
    val bt1 = s1.ballotTracker
    val bt2 = s2.ballotTracker
    bt1.prepared != bt2.prepared ||
    bt1.preparedPrime != bt2.preparedPrime ||
    bt1.commit != bt2.commit
  }
}

object SlotServiceHandler {
  trait Implicits {
    implicit val slotServiceHandler: SlotServiceHandler = new SlotServiceHandler
  }
}
