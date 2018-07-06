package bigknife.scalap
package interpreter
package service

import bigknife.scalap.ast.service.NominateService
import bigknife.scalap.ast.types.Message.Nomination
import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import implicits._
import bigknife.scalap.util._
import org.slf4j.LoggerFactory

class NominateServiceHandler extends NominateService.Handler[Stack] {
  private val log = LoggerFactory.getLogger(getClass)

  override def findRoundLeaders(tracker: NominateTracker,
                                 quorumSet: QuorumSet,
                                round: Int,
                                slotIndex: SlotIndex,
                                previousValue: Value): Stack[NominateTracker] =
    Stack {
      val neighbors: Set[NodeID] =
        quorumSet.neighbors(round, slotIndex, previousValue)
      def nodePriority(nodeID: NodeID): Long =
        util.gi(util.Gi.Mode.Mode_Priority, nodeID, round, slotIndex, previousValue)

      // find the biggest priority
      val res = neighbors.foldLeft((Set.empty[NodeID], 0L)) { (acc, n) =>
        val p = nodePriority(n)
        if (p >= acc._2) (Set(n), p)
        else if (p == acc._2) (acc._1 + n, p)
        else acc
      }

      log.debug("findRoundLeaders found the top priority in round {} is {}", round, res._2)
      tracker.copy(roundLeaders = res._1)
    }

  override def nominateNewValuesWithLeaders(tracker: NominateTracker,
                                            nodeID: NodeID,
                                            tryToNominate: Value): Stack[NominateNewValuesResult] =
    Stack { setting =>
      val leaders = tracker.roundLeaders
      if (leaders.contains(nodeID)) {
        val t = tracker.copy(
          nominationStarted = true,
          round = tracker.round + 1,
          nomination = Message
            .nominationBuilder()
            .fromNomination(tracker.nomination)
            .vote(tryToNominate)
            .build()
        )
        log.debug(
          s"current node($nodeID) is a leader, vote the value, then tracker turns into $tracker")
        BoolResult(t, successful = true)
      } else {
        val nominations: Vector[Nomination] =
          tracker.latestNominations
            .filter(x => leaders.contains(x._1))
            .values
            .map(_.statement.message)
            .toVector
        val allValues: Vector[Value] =
          nominations.flatMap(x => x.voted.toVector ++ x.accepted.toVector)
        // extract value would be ignored
        import setting.connect
        // initial value is valueOption and value hash
        val nominatingValue = allValues
          .foldLeft((Option.empty[Value], 0L)) { (acc, n) =>
            val validValue = connect.validateValue(n) match {
              case Validity.Invalid        => None
              case Validity.MaybeValid     => connect.extractValidValue(n)
              case Validity.FullyValidated => Option(n)
            }
            // if has voted, ignore it
            if (validValue.isEmpty || tracker.nomination.voted.contain(validValue.get)) acc
            else {
              val vv = validValue.get
              val curHash =
                hasher.computeValueHash(tracker.slotIndex, tracker.previousValue, tracker.round, vv)
              if (curHash > acc._2) (validValue, curHash)
              else acc
            }
          }
          ._1
        if (nominatingValue.isDefined) {
          BoolResult(
            tracker.copy(
              round = tracker.round + 1,
              nominationStarted = true,
              nomination = Message
                .nominationBuilder()
                .fromNomination(tracker.nomination)
                .vote(nominatingValue.get)
                .build()
            ),
            successful = true
          )
        } else
          BoolResult(tracker.copy(round = tracker.round + 1, nominationStarted = true),
                     successful = false)
      }
    }

  override def getNewValueFromNomination(tracker: NominateTracker,
                                         nom: Nomination): Stack[Option[Value]] = Stack { setting =>
    val allValues: Vector[Value] = nom.voted.toVector ++ nom.accepted.toVector
    // extract value would be ignored
    import setting.connect
    // initial value is valueOption and value hash
    allValues
      .foldLeft((Option.empty[Value], 0L)) { (acc, n) =>
        val validValue = connect.validateValue(n) match {
          case Validity.Invalid        => None
          case Validity.MaybeValid     => connect.extractValidValue(n)
          case Validity.FullyValidated => Option(n)
        }
        // if has voted, ignore it
        if (validValue.isEmpty) acc
        else {
          val vv = validValue.get
          val curHash =
            hasher.computeValueHash(tracker.slotIndex, tracker.previousValue, tracker.round, vv)
          if (curHash > acc._2) (validValue, curHash)
          else acc
        }
      }
      ._1
  }

  override def createNominationEnvelope(nodeID: NodeID,
                                        slotIndex: SlotIndex,
                                        quorumSet: QuorumSet,
                                        nomination: Nomination): Stack[Envelope[Nomination]] =
    Stack { setting =>
      val statement = Statement.Nominate(
        nodeID,
        slotIndex,
        quorumSet.hash,
        nomination
      )
      val signature: Signature =
        setting.connect.signData(statement.bytes, nodeID)
      Envelope.NominationEnvelope(statement, signature)
    }

  override def broadcastEnvelope(tracker: NominateTracker,
                                 envelope: Envelope[Nomination]): Stack[NominateTracker] = Stack {
    setting =>
      if (tracker.lastSentEnvelope.isEmpty ||
          Statement.newerThan(tracker.lastSentEnvelope.get.statement, envelope.statement)) {
        setting.connect.broadcastMessage(envelope)
        tracker.sentEnvelope(envelope)
      } else tracker
  }

  override def recordEnvelope(tracker: NominateTracker,
                              envelope: Envelope[Nomination]): Stack[NominateTracker] = Stack {
    tracker.copy(
      latestNominations = tracker.latestNominations + (envelope.statement.nodeID -> envelope))
  }

  override def combineValues(valueSet: ValueSet): Stack[Value] = Stack { setting =>
    setting.connect.combineValues(valueSet)
  }

  override def stopNomination(tracker: NominateTracker): Stack[NominateTracker] = Stack {
    tracker.copy(nominationStarted = false)
  }
}

object NominateServiceHandler {
  private lazy val _instance: NominateServiceHandler =
    new NominateServiceHandler
  trait Implicits {
    implicit lazy val nominateServiceHandler: NominateServiceHandler = _instance
  }
  object implicits extends Implicits
}
