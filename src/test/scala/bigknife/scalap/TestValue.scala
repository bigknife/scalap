package bigknife.scalap

import bigknife.scalap.ast.types.Value

case class TestValue(words: String) extends Value{
  override def compare(that: Value): Int = that match {
    case TestValue(w1) => words.compare(w1)
    case _ => 1
  }

  override def bytes: Array[Byte] = words.getBytes

  override def toString: String = s"TestValue($words)"
}
