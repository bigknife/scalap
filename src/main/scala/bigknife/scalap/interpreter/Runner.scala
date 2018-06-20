package bigknife.scalap.interpreter

trait Runner extends Handlers {
  import bigknife.sop._
  import bigknife.sop.implicits._
  import bigknife.scalap.ast.usecase.component._
  import bigknife.scalap.ast.usecase.component.Model._

  def runStack[A](p: SP[Model.Op, A]): Stack[A] = p.interpret[Stack]
  def runIO[A](p: SP[Model.Op, A], setting: Setting): cats.effect.IO[A] =
    runStack(p)(setting)
  def runIOAttempt[A](
                       p: SP[Model.Op, A],
                       setting: Setting): cats.effect.IO[Either[Throwable, A]] =
    runStack(p)(setting).attempt
}