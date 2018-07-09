package bigknife.scalap

import bigknife.scalap.ast.types._
import bigknife.scalap.world.Connect

class TestConnect extends Connect{
  /**
    * try to extract a valid value from a not full validated value
    *
    * @param value value not full validated
    * @return if possible, Some extracted value, or None
    */
  override def extractValidValue(value: Value): Option[Value] = None

  /**
    * application level validation
    *
    * @param value value
    * @return full/ maybe/ invalid
    */
  override def validateValue(value: Value): Value.Validity = Value.Validity.FullyValidated

  /**
    * make a signature for data of a node
    *
    * @param bytes  data
    * @param nodeID node id
    * @return
    */
override def signData(bytes: Array[Byte], nodeID: NodeID): Signature = Signature.empty

  /**
    * broadcast message
    *
    * @param envelope envelope
    */
  override def broadcastMessage[M <: Message](envelope: Envelope[M]): Unit = {
    println("==========================================")
    println(s"    Broadcast Message: ${envelope.statement.nodeID}-${envelope.statement.slotIndex}")
    envelope.statement.message match {
      case x: Message.Nomination =>
        println(s"    Nomination: ${x.voted}")
      case x: Message.Prepare =>
        println(s"    Prepare: ")
        println(s"        ballot = ${x.ballot}")
        println(s"        prepared = ${x.prepared}")
        println(s"        preparedPrime = ${x.preparedPrime}")
        println(s"        hCounter = ${x.hCounter}")
        println(s"        cCounter = ${x.cCounter}")
      case x: Message.Commit =>
        println(s"    Commit:")
        println(s"        ballot = ${x.ballot}")
        println(s"        hCounter = ${x.hCounter}")
        println(s"        cCounter = ${x.cCounter}")
        println(s"        preparedCounter = ${x.preparedCounter}")
      case x: Message.Externalize =>
        println(s"    Externalize:")
        println(s"       commit = ${x.commit}")
        println(s"       hCounter = ${x.hCounter}")
    }
    println("==========================================")
  }

  /**
    * verify signature of an envelope
    *
    * @param envelope envelope
    * @return
    */
  override def verifySignature[M <: Message](envelope: Envelope[M]): Boolean = true

  /**
    * combine value set to one value
    *
    * @param valueSet values
    * @return
    */
  override def combineValues(valueSet: ValueSet): Value = {
    // value should be test value
    valueSet.foldLeft(TestValue("")) {(acc, n) =>
      n match {
        case TestValue(words) => acc.copy(acc.words + words)
        case _ => acc
      }
    }
  }

  /**
    * run abandon ballot with counter outside
    *
    * @param counter ballot's counter
    */
  override def runAbandonBallot(nodeID: NodeID, slotIndex: SlotIndex, counter: Int): Unit = {
    println()
    println("=====================================================")
    println(s"  run abandon ballot: $nodeID, $slotIndex, $counter")
    println("=====================================================")
    println()
  }

  override def valueExternalized(nodeID: NodeID, slotIndex: SlotIndex, value: Value): Unit = {
    println()
    println("=====================================================")
    println(s"  value externalized: $nodeID, $slotIndex, $value")
    println("=====================================================")
    println()
  }
}
