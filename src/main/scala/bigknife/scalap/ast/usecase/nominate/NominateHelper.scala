package bigknife.scalap.ast.usecase.nominate

import bigknife.scalap.ast.types._
import bigknife.scalap.ast.usecase.{ConvenienceSupport, ModelSupport, NominateBaseHelper}
import bigknife.sop._
import bigknife.sop.implicits._

trait NominateHelper[F[_]] extends NominateBaseHelper[F]{
  self: NominationCore[F] with ModelSupport[F] with ConvenienceSupport[F] =>

  import self.model._

  /**
    * create a nomination message for slot[index] in the node
    * @param nodeID the node message sent from
    * @param slotIndex slot index
    * @param quorumSet node's quorum set
    * @param result nominate value result
    * @return if result param is successful, build an envelope, or return a fake envelope with failed result
    */
  protected def createNominationMessage(
      nodeID: NodeID,
      slotIndex: SlotIndex,
      quorumSet: QuorumSet,
      result: NominateNewValuesResult): SP[F, NominationEnvelopeResult] = {
    if (result.successful) {
      for {
        env <- nominateService.createNominationEnvelope(nodeID,
                                                        slotIndex,
                                                        quorumSet,
                                                        result.data.nomination)
      } yield BoolResult(env, successful = true)
    } else BoolResult(Envelope.fakeNominate, successful = false).pureSP[F]
  }
}
