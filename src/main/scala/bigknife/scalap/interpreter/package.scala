package bigknife.scalap

import cats.data.Kleisli
import cats.effect.IO

import interpreter.service._
import interpreter.store._

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
      extends MessageServiceHandler.Implicits
      with SlotServiceHandler.Implicits
      with SlotStoreHandler.Implicits
      with QuorumSetServiceHandler.Implicits
      with QuorumSetStoreHandler.Implicits
      with LogServiceHandler.Implicits
      with NodeStoreHandler.Implicits

  object runner {
    import bigknife.sop._
    import bigknife.sop.implicits._
    import bigknife.scalap.ast.usecase.component._
    import bigknife.scalap.interpreter.handlers._
    import bigknife.scalap.ast.usecase.component.Model._

    def runStack[A](p: SP[Model.Op, A]): Stack[A] = p.interpret[Stack]
    def runIO[A](p: SP[Model.Op, A], setting: Setting): cats.effect.IO[A] =
      runStack(p)(setting)
    def runIOAttempt[A](
        p: SP[Model.Op, A],
        setting: Setting): cats.effect.IO[Either[Throwable, A]] =
      runStack(p)(setting).attempt
  }

}
