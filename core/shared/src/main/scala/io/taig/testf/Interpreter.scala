package io.taig.testf

import cats.effect.implicits._
import cats.effect.{Effect, IO}
import cats.implicits._
import cats.{Eval, Functor, Id, Parallel}
import simulacrum.typeclass

import scala.annotation.implicitNotFound

@typeclass
trait Interpreter[F[_]] {
  def interpret[A](test: Test[F, A]): IO[Test[Pure, A]]
}

abstract class DefaultInterpreter[F[_]: Functor] extends Interpreter[F] {
  override final def interpret[A](test: Test[F, A]): IO[Test[Pure, A]] =
    test match {
      case test: Test.And[F, A]  => traverse(test.tests).map(Test.and)
      case test: Test.Eval[F, A] => eval(test.test.map(interpret[A]))
      case test: Test.Error      => IO.pure(test)
      case test: Test.Failure    => IO.pure(test)
      case test: Test.Label[F, A] =>
        interpret(test.test).map(Test.Label(test.description, _))
      case test: Test.Message[F, A] =>
        interpret(test.test).map(Test.Message(test.description, _))
      case test: Test.Not[F, A]  => interpret(test.test).map(Test.Not.apply)
      case test: Test.Or[F, A]   => traverse(test.tests).map(Test.or)
      case test: Test.Skip[F, A] => interpret(test.test).map(Test.Skip.apply)
      case test: Test.Success[A] => IO.pure(test)
    }

  protected def traverse[A](tests: List[Test[F, A]]): IO[List[Test[Pure, A]]]

  protected def eval[A](test: F[IO[Test[Pure, A]]]): IO[Test[Pure, A]]
}

abstract class SeqInterpreter[F[_]: Functor] extends DefaultInterpreter[F] {
  override protected final def traverse[A](
      tests: List[Test[F, A]]
  ): IO[List[Test[Pure, A]]] =
    tests.traverse(interpret[A])
}

@implicitNotFound(
  "Could not find an instance of ParInterpreter for ${F}. " +
    "Do you have a Parallel[IO] in scope?"
)
abstract class ParInterpreter[F[_]: Functor](implicit P: Parallel[IO])
    extends DefaultInterpreter[F] {
  override protected final def traverse[A](
      tests: List[Test[F, A]]
  ): IO[List[Test[Pure, A]]] =
    tests.parTraverse(interpret[A])
}

object Interpreter extends Interpreter1 {
  implicit val id: SeqInterpreter[Id] = new SeqInterpreter[Id] {
    override def eval[A](test: Id[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      test
  }

  implicit def parEffect[F[_]: Effect](
      implicit P: Parallel[IO]
  ): ParInterpreter[F] = new ParInterpreter[F] {
    override def eval[A](test: F[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      test.toIO.flatten.handleError(Test.failure)
  }

  implicit val eval: SeqInterpreter[Eval] = new SeqInterpreter[Eval] {
    override def eval[A](test: Eval[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      IO.eval(test).flatten
  }

  implicit val pure: Interpreter[Pure] = new Interpreter[Pure] {
    override def interpret[A](test: Test[Pure, A]): IO[Test[Pure, A]] =
      IO.pure(test)
  }
}

trait Interpreter1 {
  implicit def seqEffect[F[_]: Effect](
      implicit P: Parallel[IO]
  ): SeqInterpreter[F] = new SeqInterpreter[F] {
    override def eval[A](test: F[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      test.toIO.flatten.handleError(Test.failure)
  }
}
