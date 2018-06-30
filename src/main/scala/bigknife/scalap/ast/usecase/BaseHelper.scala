package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.nominate.NominationCore
import bigknife.sop._
import bigknife.sop.implicits._

trait BaseHelper[F[_]] {
  self: NominationCore[F] with ModelSupport[F] =>
  import model._

  protected def emitNominationMessage(
      nodeID: NodeID,
      tracker: NominateTracker,
      msgResult: NominationEnvelopeResult): SP[F, NominateTracker] = {
    if (msgResult.successful) {
      for {
        st <- self.processEnvelope(nodeID, msgResult.data)
        ret <- if (st == Envelope.State.Valid) for {
          nt <- nominateService.broadcastEnvelope(tracker, msgResult.data): SP[F, NominateTracker]
        } yield nt
        else tracker.pureSP[F]
      } yield ret
    } else tracker.pureSP[F]
  }

  // federated accept a value
  // two thresholds:
  // 1. vblocking set has accepted it
  // 2. quorum has ratified it
  protected def federatedAccept[M <: Message](
      value: Value,
      quorumSet: QuorumSet,
      known: Map[NodeID, Envelope[M]],
      votePredicate: StatementPredicate[M],
      acceptPredicate: StatementPredicate[M]): SP[F, Boolean] = {
    // if accepted nodes can be made up a vblocking set, it' good
    val acceptedNodes: Set[NodeID] = known
      .filter {
        case (_, envelope) =>
          acceptPredicate(envelope.statement)
      }
      .keys
      .toSet

    if (quorumSet.isVBlocking(acceptedNodes)) true.pureSP[F]
    else {
      federatedRatify(value,
                      quorumSet,
                      known,
                      (x: Statement[M]) => votePredicate(x) || acceptPredicate(x))
    }
  }

  protected def federatedRatify[M <: Message](
      value: Value,
      quorumSet: QuorumSet,
      known: Map[NodeID, Envelope[M]],
      votePredicate: StatementPredicate[M]): SP[F, Boolean] = {
    // then we should check if there is a quorum has ratified the value
    // ratify means vote or accept
    val ratifiedNodes: Set[NodeID] = known
      .filter {
        case (_, envelope) =>
          val st = envelope.statement
          votePredicate(st)
      }
      .keys
      .toSet

    // **NOTE** if the node has accepted, but can't be convinced that
    // the `accepted` should be ratified by **the node**'s quorumset.
    // we should check and filter them.
    def liftAndFilterRatifiedNodes(nodes: Set[NodeID]): SP[F, Set[NodeID]] = {
      nodes
        .foldLeft((Set.empty[NodeID], nodes).pureSP[F]) { (acc, n) =>
          for {
            pre <- acc
            qs  <- nodeStore.getQuorumSetFromStatement(known(n).statement): SP[F, QuorumSet]
          } yield if (qs.isQuorumSlice(nodes)) (pre._1 + n, pre._2 - n) else pre
        }
        .map(_._1)
    }

    liftAndFilterRatifiedNodes(ratifiedNodes).map(quorumSet.isQuorumSlice)
  }

}
