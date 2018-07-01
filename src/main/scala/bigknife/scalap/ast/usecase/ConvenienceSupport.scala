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
}
