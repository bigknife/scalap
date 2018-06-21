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

  /**
    * validate a nomination value's validity
    * @param value value
    * @return
    */
  def validateNominationValue(value: Value): SP[F, Value.Validity]

  /**
    * validate a ballot value's validity
    * @param value value
    * @return
    */
  def validateBallotValue(value: Value): SP[F, Value.Validity]

  /**
    * try to transforms a value to a fully validted value that the local node would agree to
    * @param slot slot
    * @param value value(not fully validated)
    * @return
    */
  def extractValidValue(slot: Slot, value: Value): SP[F, Option[Value]]

  /**
    * emit message to other nodes
    * @param message message
    * @return
    */
  def emitMessage(message: StatementMessage): SP[F, Unit]

  /**
    * get quorum set from a statement
    * @param statement statement
    * @return
    */
  final def getQuorumSetFromStatement(statement: Message.Statement): SP[F, Option[QuorumSet]] = {
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
  final def federatedAccept(slot: Slot,
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

    val ratifiedMessages: Map[Node.ID, StatementMessage] = messages
      .filter {
        case (_, statementMessage) =>
          accepted(statementMessage.statement) || voted(statementMessage.statement)
      }

    // exclude the nodes whose quorumset not including these nodes as a quorum slice
    // proved node has accepted and nodes' quorum set also accepted
    def ratifiedNodes(ratifiedMessages: Map[Node.ID, StatementMessage]): SP[F, Vector[Node.ID]] = {
      // result, nodes to build quorum slice
      val nodes = ratifiedMessages.keys.toVector
      val init  = (Vector.empty[Node.ID], nodes).pureSP[F]
      nodes
        .foldLeft(init) { (acc, n) =>
          val n1: SP[F, (Vector[Node.ID], Vector[Node.ID])] = for {
            prev  <- acc
            qsOpt <- getQuorumSetFromStatement(ratifiedMessages(n).statement)
            isQs <- if (qsOpt.isDefined)
              quorumSetService.isQuorumSlice(qsOpt.get, prev._2): SP[F, Boolean]
            else false.pureSP[F]
          } yield if (isQs) (prev._1 :+ n, nodes) else (prev._1, nodes.filter(_ != n))
          n1
        }
        .map(_._1)
    }

    for {
      qs                  <- quorumSetService.quorumFunction(slot.nodeId)
      acceptedByVBlocking <- quorumSetService.isVBlocking(qs, acceptedNodes)
      _ <- logService.info(s"accepted($acceptedNodes} by vblocking of $qs ? $acceptedByVBlocking")
      acceptedByQuorum <- if (acceptedByVBlocking) true.pureSP[F]
      else
        for {
          nodes    <- ratifiedNodes(ratifiedMessages)
          accepted <- quorumSetService.isQuorumSlice(qs, nodes)
          _ <- logService.info(s"ratified($nodes) is a quorum slice of $qs ? $accepted")
        } yield accepted
    } yield acceptedByQuorum

  }

  final def federatedRatify(slot: Slot,
                            voted: StatementPredict,
                            messages: Map[Node.ID, StatementMessage]): SP[F, Boolean] = {
    val ratifiedMessages = messages
      .filter {
        case (_, statementMessage) =>
          voted(statementMessage.statement)
      }

    // exclude the nodes whose quorumset not including these nodes as a quorum slice
    // proved node has accepted and nodes' quorum set also accepted
    def ratifiedNodes(ratifiedMessages: Map[Node.ID, StatementMessage]): SP[F, Vector[Node.ID]] = {
      // result, nodes to build quorum slice
      val nodes = ratifiedMessages.keys.toVector
      val init  = (Vector.empty[Node.ID], nodes).pureSP[F]
      nodes
        .foldLeft(init) { (acc, n) =>
          for {
            prev  <- acc
            qsOpt <- getQuorumSetFromStatement(ratifiedMessages(n).statement)
            isQs <- if (qsOpt.isDefined)
              quorumSetService.isQuorumSlice(qsOpt.get, prev._2): SP[F, Boolean]
            else false.pureSP[F]
          } yield if (isQs) (prev._1 :+ n, nodes) else (prev._1, nodes.filter(_ != n))
        }
        .map(_._1)
    }

    for {
      qs    <- quorumSetService.quorumFunction(slot.nodeId)
      nodes <- ratifiedNodes(ratifiedMessages)
      isQs  <- quorumSetService.isQuorumSlice(qs, nodes)
    } yield isQs
  }
}
