package bigknife.scalap

import bigknife.scalap.interpreter.service._
import bigknife.scalap.interpreter.store.NodeStoreHandler
import cats.data.Kleisli
import cats.effect.IO

package object interpreter {

  type Stack[A] = Kleisli[IO, Setting, A]

  object Stack {
    def apply[A](a: => A): Stack[A] = Kleisli { _ =>
      IO(a)
    }
    def apply[A](f: Setting => A): Stack[A] = Kleisli { setting =>
      IO { f(setting) }
    }
  }

  object handlers
      extends LogServiceHandler.Implicits
      with NodeStoreHandler.Implicits
      with NominateServiceHandler.Implicits
      with BallotServiceHandler.Implicits
      with EnvelopeServiceHandler.Implicits

  object runner {
    import bigknife.sop._
    import bigknife.sop.implicits._
    import bigknife.scalap.ast.usecase.component._
    import bigknife.scalap.interpreter.handlers._
    import bigknife.scalap.ast.usecase.component.Model._

    def runStack[A](p: SP[Model.Op, A]): Stack[A] = p.interpret[Stack]
    def runIO[A](p: SP[Model.Op, A], setting: Setting): cats.effect.IO[A] =
      runStack(p)(setting)
    def runIOAttempt[A](p: SP[Model.Op, A],
                        setting: Setting): cats.effect.IO[Either[Throwable, A]] =
      runStack(p)(setting).attempt
  }

}
