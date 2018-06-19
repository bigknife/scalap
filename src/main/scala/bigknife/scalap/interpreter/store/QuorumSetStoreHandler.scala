package bigknife.scalap.interpreter
package store

import bigknife.scalap.ast.store.QuorumSetStore

class QuorumSetStoreHandler extends QuorumSetStore.Handler[Stack]{

}

object QuorumSetStoreHandler {
  trait Implicits {
    implicit val quorumSetStoreHandler: QuorumSetStoreHandler = new QuorumSetStoreHandler
  }
}
