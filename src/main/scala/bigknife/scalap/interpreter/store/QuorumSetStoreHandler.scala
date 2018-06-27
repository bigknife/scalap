package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.QuorumSetStore
import bigknife.scalap.ast.types.{Hash, QuorumSet}

class QuorumSetStoreHandler extends QuorumSetStore.Handler[Stack] {

  import scala.collection._
  private val memStore: mutable.Map[Hash, QuorumSet] = mutable.Map.empty

  override def getQuorumSet(hash: Hash): Stack[Option[QuorumSet]] = Stack {
    memStore.get(hash)
  }

  override def saveQuorumSet(hash: Hash, quorumSet: QuorumSet): Stack[Unit] = Stack {
    memStore.put(hash, quorumSet)
    ()
  }
}

object QuorumSetStoreHandler {
  private val _inst = new QuorumSetStoreHandler
  trait Implicits {
    implicit val quorumSetStoreHandler: QuorumSetStoreHandler = _inst
  }
}
