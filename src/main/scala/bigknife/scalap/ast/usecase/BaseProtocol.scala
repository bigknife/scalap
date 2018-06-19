package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

/**
  * base protocol, provide `model` for sub classes.
  */
trait BaseProtocol[F[_]] {
  // result of running protocol
  type Result = (Slot, MessageState)
  def validResult(slot: Slot): SP[F, Result] =
    (slot, Message.State.valid).pureSP[F]
  def invalidResult(slot: Slot): SP[F, Result] =
    (slot, Message.State.invalid).pureSP[F]

  val model: component.Model[F]

  import model._

  def getQuorumSetFromStatement(statement: Message.Statement): SP[F, Option[QuorumSet]] = {
    statement match {
      case x: Message.Externalize =>
        for {
          qs <- quorumSetService.buildSingletonQuorumSet(x.nodeId)
        } yield Option(qs)

      case x =>
        for {
        qsOpt <- quorumSetStore.getQuorumSet(x.quorumSetHash)
      } yield qsOpt

    }
  }

  /**
    * federated voting for accept a statmenet
    * @param slot slot
    * @param voted voted predict
    * @param accepted accepted predict
    * @param messages messages
    * @return
    */
  def federateAccept(
      slot: Slot,
      voted: StatementPredict,
      accepted: StatementPredict,
      messages: Map[Node.ID, StatementMessage]): SP[F, Boolean] = {
    // 1. if a node v's vblocking has accepted, then accepted.
    //    say, if accepted nodes is the v-blocking set of the quorum set, then accepted
    val acceptedNodes = messages
      .filter {
        case (_, statementMessage) => accepted(statementMessage.statement)
      }
      .keys
      .toVector

    val ratifiedMessages = messages
      .filter {
        case (_, statementMessage) =>
          accepted(statementMessage.statement) || voted(
            statementMessage.statement)
      }

    // exclude the nodes whose quorumset not including these nodes as a quorum slice
    // proved node has accepted and nodes' quorum set also accepted
    def ratifiedNodes(ratifiedMessages: Map[Node.ID, StatementMessage]): SP[F, Vector[Node.ID]] = {
      // result, nodes to build quorum slice
      val nodes = ratifiedMessages.keys.toVector
      val init = (Vector.empty[Node.ID], nodes).pureSP[F]
      nodes.foldLeft(init) {(acc, n) =>
        for {
          prev <- acc
          qsOpt <- getQuorumSetFromStatement(ratifiedMessages(n).statement)
          isQs <- if (qsOpt.isDefined) quorumSetService.isQuorumSlice(qsOpt.get, prev._2) else false.pureSP[F]
        } yield if (isQs) (prev._1 :+ n, nodes) else (prev._1, nodes.filter(_ != n))
      }.map(_._1)
    }


    for {
      qs <- quorumSetService.quorumFunction(slot.nodeId)
      acceptedByVBlocking <- quorumSetService.isVBlocking(qs, acceptedNodes)
      acceptedByQuorum <- if (acceptedByVBlocking) true.pureSP[F] else for {
        nodes <- ratifiedNodes(messages)
        accepted <- quorumSetService.isQuorumSlice(qs, nodes)
      } yield accepted
    } yield acceptedByQuorum

  }
}
