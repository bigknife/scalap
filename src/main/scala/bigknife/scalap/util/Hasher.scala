package bigknife.scalap.util

import java.nio.ByteBuffer

import bigknife.scalap.ast.types._

trait Hasher extends Sha3 {
  private def bytesOfInt(i: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(i)
    bb.array()
  }
  def computeValueHash(slotIndex: SlotIndex, prev: Value, round: Int, value: Value): Long = {
    val hash_K = 3
    val source = slotIndex.bytes ++ prev.bytes ++ bytesOfInt(hash_K) ++ bytesOfInt(round) ++ value.bytes
    BigInt(sha3(source)).toLong
  }

  def computeNodeHash(slotIndex: SlotIndex,
                      prev: Value,
                      isPriority: Boolean,
                      round: Int,
                      nodeID: NodeID): Long = {
    val hash_P = 2
    val hash_N = 1
    val source = slotIndex.bytes ++ prev.bytes ++ bytesOfInt(if (isPriority) hash_P else hash_N) ++ bytesOfInt(
      round) ++ nodeID.bytes
    BigInt(sha3(source)).toLong
  }
}
