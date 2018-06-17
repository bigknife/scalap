package bigknife.scalap

import bigknife.scalap.ast.usecase._
import bigknife.scalap.ast.types._
import bigknife.scalap.interpreter._
import org.scalatest.FunSuite

class NominationProtocolSpec extends FunSuite {
  test("test program") {
    info(s"scp is $scp")
    val nodeId = Hash(Array.emptyByteArray)
    val msg = Message(
      Message.Nominate(nodeId, 0, Vector.empty, Vector.empty, Hash(Array.emptyByteArray)),
      Signature(Array.emptyByteArray)
    )
    val p = scp.handleMessage(nodeId, msg)
    runner.runIO(p, Setting()).unsafeRunSync()
  }
}
