package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.implicits._

trait ConvenienceSupport[F[_]] {
  self: ModelSupport[F] =>
  import model._

  def ifM[A](data: A, p: A => Boolean)(spf: A => SP[F, A]): SP[F, A] =
    if (p(data)) spf(data) else data.pureSP[F]

  // **NOTE** if the node has accepted, but can't be convinced that
  // the `accepted` should be ratified by **the node**'s quorumset.
  // we should check and filter them.
  def liftAndFilterRatifiedNodes[M <: Message](
      nodes: Set[NodeID],
      known: Map[NodeID, Envelope[M]]): SP[F, Set[NodeID]] = {
    nodes
      .foldLeft((Set.empty[NodeID], nodes).pureSP[F]) { (acc, n) =>
        for {
          pre <- acc
          qs  <- nodeStore.getQuorumSetFromStatement(known(n).statement): SP[F, QuorumSet]
        } yield if (qs.isQuorumSlice(nodes)) (pre._1 + n, pre._2 - n) else pre
      }
      .map(_._1)
  }

  // federated accept a value
  // two thresholds:
  // 1. vblocking set has accepted it
  // 2. quorum has ratified it
  protected def federatedAccept[M <: Message](
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
      federatedRatify(quorumSet, known, (x: Statement[M]) => votePredicate(x) || acceptPredicate(x))
    }
  }

  protected def federatedRatify[M <: Message](
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
    /*
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
     */
    liftAndFilterRatifiedNodes(ratifiedNodes, known).map(quorumSet.isQuorumSlice)
  }
}
