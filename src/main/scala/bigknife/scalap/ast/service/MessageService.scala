package bigknife.scalap.ast.service

import bigknife.scalap.ast.types.Message.NominationStatement
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
}
