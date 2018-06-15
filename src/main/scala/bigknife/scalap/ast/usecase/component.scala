package bigknife.scalap.ast.usecase

import bigknife.scalap.ast.service.SlotService
import bigknife.scalap.ast.store.SlotStore
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
  }
}
