package bigknife.scalap

import java.util.TimerTask

import bigknife.scalap.ast.service.ApplicationExtension
import bigknife.scalap.ast.types.Value.Validity
import bigknife.scalap.ast.types._
import bigknife.scalap.interpreter.Stack
import org.slf4j.{Logger, LoggerFactory}

class TestApplicationExtension extends ApplicationExtension.Handler[Stack] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val timerCache: collection.mutable.Map[String, java.util.Timer] =
    collection.mutable.Map.empty
  override def combineValues(values: Vector[Value]): Stack[Value] = Stack {
    values.map(_.asInstanceOf[TestValue]).fold(TestValue("")) {
      case (TestValue(a), TestValue(b)) => TestValue(a + "\n" + b)
    }
  }

  override def validateNominationValue(value: Value): Stack[Value.Validity] = Stack {
    Validity.fullyValidated
  }

  override def validateBallotValue(value: Value): Stack[Value.Validity] = Stack {
    Validity.fullyValidated
  }

  override def extractValidValue(slot: Slot, value: Value): Stack[Option[Value]] = Stack {
    None
  }

  override def verifyMessage(message: StatementMessage): Stack[Boolean] = Stack {
    true
  }

  override def emitMessage(message: StatementMessage): Stack[Unit] = Stack {
    logger.info(s"emit message: $message")
  }

  override def startBallotProtocolTimer(slot: Slot): Stack[Unit] = Stack {
    // start timer
    logger.info("start ballot protocol timer")
  }

  override def stopBallotProtocolTimer(slot: Slot): Stack[Unit] = Stack {
    logger.info("stop ballot protocol timer")
  }

  override def ballotDidHearFromQuorum(slot: Slot, ballot: Ballot): Stack[Unit] = Stack {
    logger.info(s"$slot did hear from quorum for $ballot")
  }

  override def computeTimeoutForNomination(slot: Slot): Stack[Long] = Stack {
    // simplest , first round, 1000, second round 2000, etc. linear.
    slot.nominateTracker.roundNumber * 1000L
  }

  override def setupTimer(slot: Slot, timeout: Long, reNominateArgs: ReNominateArgs): Stack[Unit] = Stack {setting =>
    val timer = if (timerCache.contains("nomination-timer")) {
      timerCache("nomination-timer")
    } else {
      val t = new java.util.Timer("nomination-timer")
      timerCache.put("nomination-timer", t)
      t
    }

    timer.schedule(new TimerTask {
      override def run(): Unit = {
        //callback.run()
        setting.reNominate(slot, reNominateArgs)
      }

    }, timeout)
    ()
  }
}

object TestApplicationExtension {
  trait Implicits {
    implicit val applicationExtension: ApplicationExtension.Handler[Stack] =
      new TestApplicationExtension
  }
}
