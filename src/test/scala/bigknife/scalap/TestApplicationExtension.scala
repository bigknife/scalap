package bigknife.scalap

import bigknife.scalap.ast.service.ApplicationExtension
import bigknife.scalap.ast.types.Value
import bigknife.scalap.interpreter.Stack

class TestApplicationExtension extends ApplicationExtension.Handler[Stack] {
  override def combineValues(values: Vector[Value]): Stack[Value] = Stack {
    values.map(_.asInstanceOf[TestValue]).fold(TestValue("")) {
      case (TestValue(a), TestValue(b)) => TestValue(a + "\n" + b)
    }
  }
}

object TestApplicationExtension {
  trait Implicits {
    implicit val applicationExtension: ApplicationExtension.Handler[Stack] =
      new TestApplicationExtension
  }
}
