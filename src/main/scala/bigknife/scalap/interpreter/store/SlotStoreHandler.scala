package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.SlotStore
import bigknife.scalap.ast.types._
import bigknife.scalap.ast.types.{Node, Slot}

class SlotStoreHandler extends SlotStore.Handler[Stack]{
  import scala.collection._
  private val memStore: mutable.Map[Node.ID, Slot] = mutable.Map.empty

  override def getSlotOfNode(nodeId: Node.ID, slotIndex: Long): Stack[Option[Slot]] = Stack {
    memStore.get(nodeId)
  }

  override def saveSlotForNode(nodeId: Node.ID, slot: Slot): Stack[Unit] = Stack {
    memStore.put(nodeId, slot)
    ()
  }
}

object SlotStoreHandler {
  private val _inst = new SlotStoreHandler
  trait Implicits {
    implicit val slotStoreHandler: SlotStoreHandler = _inst
  }
}
