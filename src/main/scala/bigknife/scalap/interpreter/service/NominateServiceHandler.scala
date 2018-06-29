package bigknife.scalap
package interpreter
package service

import bigknife.scalap.ast.service.NominateService
import bigknife.scalap.ast.types.Message.Nomination
import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import bigknife.scalap.util._
import org.slf4j.LoggerFactory

class NominateServiceHandler extends NominateService.Handler[Stack] {
  private val log = LoggerFactory.getLogger(getClass)

  override def findRoundLeaders(quorumSet: QuorumSet,
                                round: Int,
                                slotIndex: SlotIndex,
                                previousValue: Value): Stack[Set[NodeID]] =
    Stack {
      val neighbors: Set[NodeID] =
        quorumSet.neighbors(round, slotIndex, previousValue)
      def nodePriority(nodeID: NodeID): Long =
        util.gi(util.Gi.Mode.Mode_Priority,
                nodeID,
                round,
                slotIndex,
                previousValue)

      // find the biggest priority
      val res = neighbors.foldLeft((Set.empty[NodeID], 0L)) { (acc, n) =>
        val p = nodePriority(n)
        if (p >= acc._2) (Set(n), p)
        else if (p == acc._2) (acc._1 + n, p)
        else acc
      }

      log.debug("findRoundLeaders found the top priority in round {} is {}",
                round,
                res._2)
      res._1
    }

  override def nominateNewValues(
      tracker: NominateTracker,
      nodeID: NodeID,
      tryToNominate: Value,
      leaders: Set[NodeID]): Stack[NominateNewValuesResult] = Stack { setting =>
    if (leaders.contains(nodeID)) {
      BoolResult(tracker.copy(
                   nomination = Message
                     .nominationBuilder()
                     .fromNomination(tracker.nomination)
                     .vote(tryToNominate)
                     .build()),
                 successful = true)
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
          if (validValue.isEmpty || tracker.nomination.voted.contain(
                validValue.get)) acc
          else {
            val vv = validValue.get
            val curHash =
              hasher.computeValueHash(tracker.slotIndex,
                                      tracker.previousValue,
                                      tracker.round,
                                      vv)
            if (curHash > acc._2) (validValue, curHash)
            else acc
          }
        }
        ._1
      if (nominatingValue.isDefined) {
        BoolResult(tracker.copy(
                     nomination = Message
                       .nominationBuilder()
                       .fromNomination(tracker.nomination)
                       .vote(nominatingValue.get)
                       .build()),
                   successful = true)
      } else BoolResult(tracker, successful = false)
    }
  }

  override def createNominationEnvelope(
      nodeID: NodeID,
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
        setting.connect.signData(statement.toBytes, nodeID)
      Envelope(statement, signature)
    }

  override def broadcastEnvelope(
      tracker: NominateTracker,
      envelope: Envelope[Nomination]): Stack[NominateTracker] = Stack {
    setting =>
      if (tracker.lastSentEnvelope.isEmpty ||
          envelope.statement
            .newerThan(tracker.lastSentEnvelope.get.statement)) {
        setting.connect.broadcastMessage(envelope)
        tracker.sentEnvelope(envelope)
      } else tracker
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
