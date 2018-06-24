package bigknife.scalap.interpreter
package service

import java.math.BigInteger

import bigknife.scalap.ast.service.SlotService
import bigknife.scalap.ast.types.BallotTracker.Phase
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
          latestCompositeCandidate = Some(compositeValue)
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

    //setPreparedAccept see BallotProtocol.cpp#869
    val ySlot =
      if (xSlot.ballotTracker.commit.isDefined && xSlot.ballotTracker.highBallot.isDefined) {
        val c = xSlot.ballotTracker.commit.get
        val h = xSlot.ballotTracker.highBallot.get

        if (xSlot.ballotTracker.prepared.exists(p => h <= p && !h.compatible(p)) ||
            xSlot.ballotTracker.preparedPrime.exists(p => h <= p && !h.compatible(p))) {
          require(xSlot.ballotTracker.phase == Phase.Prepare)
          xSlot.copy(
            ballotTracker = xSlot.ballotTracker.copy(
              commit = None
            ))
        } else xSlot
      } else xSlot

    ySlot
  }

  override def setPreparedConfirmed(slot: Slot,
                                    newC: Option[Ballot],
                                    newH: Option[Ballot]): Stack[Slot] = Stack {
    val newHighBallot: Option[Ballot] =
      slot.ballotTracker.highBallot.flatMap(p => newH.map(np => if (np > p) np else p)).orElse(newH)

    val newCommit = if (newC.isDefined) newC else slot.ballotTracker.commit

    slot.copy(
      ballotTracker = slot.ballotTracker.copy(
        highBallot = newHighBallot,
        commit = newCommit
      )
    )
  }

  override def setAcceptCommit(slot: Slot, commit: Ballot, high: Ballot): Stack[Slot] = Stack {
    val highOpt   = slot.ballotTracker.highBallot
    val commitOpt = slot.ballotTracker.commit
    val s0 =
      if (highOpt.isEmpty || commitOpt.isEmpty || highOpt.get != high || commitOpt.get != commit) {
        slot.copy(
          ballotTracker = slot.ballotTracker.copy(
            commit = Some(commit),
            highBallot = Some(high)
          ))
      } else slot

    val s1 = if (s0.ballotTracker.phase == Phase.Prepare) {
      s0.copy(
        ballotTracker = s0.ballotTracker.copy(
          phase = Phase.Confirm,
          preparedPrime = None
        ))
    } else s0

    s1
  }

  override def setConfirmCommit(slot: Slot, _commit: Ballot, _high: Ballot): Stack[Slot] = Stack {
    slot.copy(
      ballotTracker = slot.ballotTracker.copy(
        commit = Some(_commit),
        highBallot = Some(_high),
        phase = Phase.Externalized
      ),
      nominateTracker = slot.nominateTracker.copy(nominationStarted = false)
    )
  }

  override def setBumpBallot(slot: Slot, ballot: Ballot, bumped: Boolean): Stack[Slot] = Stack {
    val s0 = slot.copy(
      ballotTracker = slot.ballotTracker.copy(
        currentBallot = Some(ballot)
      ))
    if (bumped) s0.copy(ballotTracker = s0.ballotTracker.copy(heardFromQuorum = !bumped))
    else s0
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
    bt1.commit != bt2.commit ||
    bt1.currentBallot != bt2.currentBallot ||
    bt1.highBallot != bt2.highBallot ||
    bt1.phase != bt2.phase
  }

  override def setHeardFromQuorum(slot: Slot, heard: Boolean): Stack[Slot] = Stack {
    slot.copy(ballotTracker = slot.ballotTracker.copy(heardFromQuorum = heard))
  }

  override def emitLatestBallotMessage(slot: Slot): Stack[Slot] = Stack {
    slot.copy(ballotTracker = slot.ballotTracker.copy(
      lastEmittedMessage = slot.ballotTracker.lastMessage
    ))
  }

  override def emitBallotMessage(slot: Slot, message: BallotMessage): Stack[Slot] = Stack {
    slot.copy(
      ballotTracker = slot.ballotTracker.copy(
        lastEmittedMessage = Some(message)
      )
    )
  }
}

object SlotServiceHandler {
  trait Implicits {
    implicit val slotServiceHandler: SlotServiceHandler = new SlotServiceHandler
  }
}
