package bigknife.scalap

import bigknife.scalap.ast.service.ApplicationExtension
import bigknife.scalap.ast.types.{Ballot, Slot, StatementMessage, Value}
import bigknife.scalap.interpreter.Stack
import org.slf4j.{Logger, LoggerFactory}

class TestApplicationExtension extends ApplicationExtension.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  override def combineValues(values: Vector[Value]): Stack[Value] = Stack {
    values.map(_.asInstanceOf[TestValue]).fold(TestValue("")) {
      case (TestValue(a), TestValue(b)) => TestValue(a + "\n" + b)
    }
  }

  override def emitMessage(message: StatementMessage): Stack[Unit] = Stack {
    logger.info(s"emit message: $message")
  }

  override def startBallotProtocolTimer(slot: Slot): Stack[Unit] = Stack{
    // start timer
    logger.info("start ballot protocol timer")
  }

  override def stopBallotProtocolTimer(slot: Slot): Stack[Unit] = Stack {
    logger.info("stop ballot protocol timer")
  }

  override def ballotDidHearFromQuorum(slot: Slot, ballot: Ballot): Stack[Unit] = Stack {
    logger.info(s"$slot did hear from quorum for $ballot")
  }
}

object TestApplicationExtension {
  trait Implicits {
    implicit val applicationExtension: ApplicationExtension.Handler[Stack] =
      new TestApplicationExtension
  }
}
