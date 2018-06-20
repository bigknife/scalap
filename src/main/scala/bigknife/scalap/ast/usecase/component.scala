package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.service._
import bigknife.scalap.ast.store._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

/**
  * system component
  */
object component {
  @sps trait Model[F[_]] {
    val slotStore: SlotStore[F]
    val slotService: SlotService[F]
    val messageService: MessageService[F]
    val nodeStore: NodeStore[F]
    val quorumSetService: QuorumSetService[F]
    val quorumSetStore: QuorumSetStore[F]
    val logService: LogService[F]
  }
}
