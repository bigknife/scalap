package bigknife.scalap.ast.service

import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

/**
  * application level extension
  */
@sp trait ApplicationExtension[F[_]] {
  /**
    * validate a nomination value's validity
    * @param value value
    * @return
    */
  def validateNominationValue(value: Value): P[F, Value.Validity]
  /**
    * validate a ballot value's validity
    * @param value value
    * @return
    */
  def validateBallotValue(value: Value): P[F, Value.Validity]

  def extractValidValue(slot: Slot, value: Value): P[F, Option[Value]]

  /**
    * verify a message in application level.
    * @param message message with statement and signature
    * @return if passed true else false
    */
  def verifyMessage(message: StatementMessage): P[F, Boolean]
  /**
    * combine some values to one value
    * @param values some values
    * @return
    */
  def combineValues(values: Vector[Value]): P[F, Value]

  // ballot protocol timer
  def startBallotProtocolTimer(slot: Slot): P[F, Unit]
  def stopBallotProtocolTimer(slot: Slot): P[F, Unit]
  def ballotDidHearFromQuorum(slot: Slot, ballot: Ballot): P[F, Unit]

  /**
    * emit message to other nodes
    * @param message message
    * @return
    */
  def emitMessage(message: StatementMessage): P[F, Unit]

  /**
    * compute current slot's nomination timeout in milliseconds
    * @param slot slot
    * @return
    */
  def computeTimeoutForNomination(slot: Slot): P[F, Long]

  def setupTimer(slot: Slot, timeout: Long, callback: Callback): P[F, Unit]
}
