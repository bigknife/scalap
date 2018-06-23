package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.Message.NominationStatement
import bigknife.scalap.ast.types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

@sp trait MessageService[F[_]] {
  /**
    * compute statement to see if it's sane or not.
    * @param statement nomination statement
    * @return
    */
  def isSaneNominationStatement(statement: NominationStatement): P[F, Boolean]

  /**
    * compute statement to see if it's sane or not
    * @param statement ballot statement
    * @return
    */
  def isSaneBallotStatement(statement: BallotStatement, self: Boolean): P[F, Boolean]

  /**
    * compute two nomination statement, return a newer one.
    * @param st1 statement 1
    * @param st2 statement 2
    * @return
    */
  def newerNominationStatement(st1: NominationStatement, st2: NominationStatement): P[F, NominationStatement]

  def firstNominationStatementIsNewer(st1: NominationStatement, st2: NominationStatement): P[F, Boolean]

  def firstBallotStatementIsNewer(st1: BallotStatement, st2: BallotStatement): P[F, Boolean]

  def createNominationMessage(slot: Slot, quorumSetHash: Hash): P[F, NominationMessage]

  def createBallotMessage(slot: Slot, quorumSetHash: Hash): P[F, BallotMessage]

  def getWorkingBallot(st: BallotStatement): P[F, Ballot]

  def getPreparedCandidates(slot: Slot, st: BallotStatement): P[F, Vector[Ballot]]
}
