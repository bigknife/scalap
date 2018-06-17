package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.SlotStore

class SlotStoreHandler extends SlotStore.Handler[Stack]{

}

object SlotStoreHandler {
  trait Implicits {
    implicit val slotStoreHandler: SlotStoreHandler = new SlotStoreHandler
  }
}
