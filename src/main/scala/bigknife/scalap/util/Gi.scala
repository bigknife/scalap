package bigknife.scalap.util

import java.nio.ByteBuffer

import bigknife.scalap.ast.types.{NodeID, SlotIndex, Value}

/**
  * Gi is a SHA256 function
  */
trait Gi extends Hasher {
  def apply(mode: Gi.Mode,
            nodeID: NodeID,
            round: Int,
            slotIndex: SlotIndex,
            previousValue: Value): Long = mode match {
    case Gi.Mode.Mode_Priority =>
      computeNodeHash(slotIndex, previousValue, isPriority = true, round, nodeID)
    case _ =>
      computeNodeHash(slotIndex, previousValue, isPriority = false, round, nodeID)
  }
}

object Gi {
  sealed trait Mode {
    def value: Int
  }
  object Mode {
    object Mode_Neighbor extends Mode {
      override val value: Int = 1
    }
    object Mode_Priority extends Mode {
      override val value: Int = 2
    }
  }
}
