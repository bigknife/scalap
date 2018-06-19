package bigknife.scalap.interpreter
package service

import bigknife.scalap.ast.service.SlotService
import bigknife.scalap.ast.types.{Message, NominationMessage, Slot, Value}

class SlotServiceHandler extends SlotService.Handler[Stack] {
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
}

object SlotServiceHandler {
  trait Implicits {
    implicit val slotServiceHandler: SlotServiceHandler = new SlotServiceHandler
  }
}
