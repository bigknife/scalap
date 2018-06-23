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
}
