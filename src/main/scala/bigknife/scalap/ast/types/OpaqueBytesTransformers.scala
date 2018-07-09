package bigknife.scalap.ast.types

import java.nio.ByteBuffer
import OpaqueBytes.OpaqueBytesTransformer
import implicits._

trait OpaqueBytesTransformers {
  implicit val IntOpaqueBytesTransformer: OpaqueBytesTransformer[Int] =
    OpaqueBytesTransformer[Int] { i =>
      val b = ByteBuffer.allocate(4)
      b.putInt(i)
      b.array()
    }

  implicit val LongOpaqueBytesTransformer: OpaqueBytesTransformer[Long] =
    OpaqueBytesTransformer[Long] { i =>
      val b = ByteBuffer.allocate(8)
      b.putLong(i)
      b.array()
    }

  implicit val BallotOpaqueBytesTransformer: OpaqueBytesTransformer[Ballot] =
    OpaqueBytesTransformer[Ballot] { b =>
      val bb = java.nio.ByteBuffer.allocate(4)
      bb.putInt(b.counter)
      bb.array() ++ b.value.bytes
    }

  implicit val ValueSetOpaqueBytesTransformer: OpaqueBytesTransformer[ValueSet] =
    OpaqueBytesTransformer[ValueSet] { vs =>
      val v: Vector[Array[Byte]] = vs.toVector.map(_.bytes)
      v.foldLeft(Array.emptyByteArray) { _ ++ _ }
    }

  implicit val NominateStatementBytesTransformer: OpaqueBytesTransformer[Statement.Nominate] =
    OpaqueBytesTransformer[Statement.Nominate] { n =>
      n.nodeID.bytes ++ n.slotIndex.bytes ++ n.quorumSetHash.bytes ++
        n.message.voted.bytes ++ n.message.accepted.bytes
    }

  implicit val PrepareStatementBytesTransformer: OpaqueBytesTransformer[Statement.Prepare] =
    OpaqueBytesTransformer[Statement.Prepare] { n =>
      n.nodeID.bytes ++ n.slotIndex.bytes ++ n.quorumSetHash.bytes ++
        n.message.ballot.bytes ++ n.message.prepared.bytes ++ n.message.preparedPrime.bytes ++
        n.message.cCounter.bytes ++ n.message.hCounter.bytes
    }

  implicit val CommitStatementBytesTransformer: OpaqueBytesTransformer[Statement.Commit] =
    OpaqueBytesTransformer[Statement.Commit] { n =>
      n.nodeID.bytes ++ n.slotIndex.bytes ++ n.quorumSetHash.bytes ++
        n.message.ballot.bytes ++ n.message.preparedCounter.bytes ++
        n.message.cCounter.bytes ++ n.message.hCounter.bytes
    }

  implicit val ExternalizeStatementBytesTransformer: OpaqueBytesTransformer[Statement.Externalize] =
    OpaqueBytesTransformer[Statement.Externalize] { n =>
      n.nodeID.bytes ++ n.slotIndex.bytes ++ n.quorumSetHash.bytes ++ n.message.hCounter.bytes
    }

  implicit val BallotStatementTransformer: OpaqueBytesTransformer[BallotStatement[BallotMessage]] =
    OpaqueBytesTransformer[BallotStatement[BallotMessage]] {
      case x: Statement.Prepare => PrepareStatementBytesTransformer.f(x)
      case x: Statement.Commit => CommitStatementBytesTransformer.f(x)
      case x: Statement.Externalize => ExternalizeStatementBytesTransformer.f(x)
    }
}
