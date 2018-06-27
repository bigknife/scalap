package bigknife.scalap

import java.math.BigInteger

import bigknife.scalap.ast.types.Value

case class TestValue(str: String) extends Value {
  override def orderFactor: Long = new BigInteger(str.getBytes).longValue()
  override def asBytes: Array[Byte] = str.getBytes("utf-8")
}
