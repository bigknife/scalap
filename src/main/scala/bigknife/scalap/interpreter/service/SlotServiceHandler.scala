package bigknife.scalap.interpreter
package service

import java.math.BigInteger

import bigknife.scalap.ast.service.SlotService
import bigknife.scalap.ast.types.Node.ID
import bigknife.scalap.ast.types._
import org.bouncycastle.jcajce.provider.digest.SHA3

class SlotServiceHandler extends SlotService.Handler[Stack] {
  import scala.collection._

  private val latestNominationStore: mutable.Map[(Node.ID, Long), NominationStatement] = mutable.Map.empty

  override def trackNewNominationMessage(slot: Slot,
                                         nominationMessage: NominationMessage): Stack[Slot] =
    Stack {
      slot.copy(
        nominateTracker = slot.nominateTracker.copy(
          latestNominations = slot.nominateTracker.latestNominations + (slot.nodeId -> nominationMessage)),
        statementHistory = slot.statementHistory :+ Message.HistoricalStatement(
          nominationMessage.statement,
          System.currentTimeMillis(),
          slot.fullValidated)
      )
    }

  override def acceptNomination(slot: Slot, value: Value): Stack[Slot] = Stack {
    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        voted = slot.nominateTracker.voted :+ value,
        accepted = slot.nominateTracker.accepted :+ value
      )
    )
  }

  override def voteNomination(slot: Slot, value: Value): Stack[Slot] = Stack {
    slot.copy(
      nominateTracker = slot.nominateTracker.copy(
        voted = slot.nominateTracker.voted :+ value
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
}

object SlotServiceHandler {
  trait Implicits {
    implicit val slotServiceHandler: SlotServiceHandler = new SlotServiceHandler
  }
}
