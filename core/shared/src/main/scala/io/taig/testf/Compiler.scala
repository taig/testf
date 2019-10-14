package io.taig.testf

import cats.effect.implicits._
import cats.effect.{Effect, IO}
import cats.implicits._
import cats.{Eval, Functor, Id}
import simulacrum.typeclass

@typeclass
trait Compiler[F[_]] {
  def compile[A](test: Test[F, A]): IO[Test[Pure, A]]
}

object Compiler {
  abstract class DefaultCompiler[F[_]: Functor] extends Compiler[F] {
    override final def compile[A](test: Test[F, A]): IO[Test[Pure, A]] =
      test match {
        case test: Test.And[F, A] =>
          test.tests.traverse(compile[A]).map(Test.and)
        case test: Test.Eval[F, A] => eval(test.test.map(compile[A]))
        case test: Test.Error      => IO.pure(test)
        case test: Test.Failure    => IO.pure(test)
        case test: Test.Label[F, A] =>
          compile(test.test).map(Test.Label(test.description, _))
        case test: Test.Message[F, A] =>
          compile(test.test).map(Test.Message(test.description, _))
        case test: Test.Not[F, A]  => compile(test.test).map(Test.Not.apply)
        case test: Test.Or[F, A]   => test.tests.traverse(compile).map(Test.or)
        case test: Test.Skip[F, A] => compile(test.test).map(Test.Skip.apply)
        case test: Test.Success[A] => IO.pure(test)
      }

    def eval[A](test: F[IO[Test[Pure, A]]]): IO[Test[Pure, A]]
  }

  implicit val id: Compiler[Id] = new DefaultCompiler[Id] {
    override def eval[A](test: Id[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      test
  }

  implicit def effect[F[_]: Effect]: Compiler[F] = new DefaultCompiler[F] {
    override def eval[A](test: F[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      test.toIO.flatten.handleError(Test.failure)
  }

  implicit val eval: Compiler[Eval] = new DefaultCompiler[Eval] {
    override def eval[A](test: Eval[IO[Test[Pure, A]]]): IO[Test[Pure, A]] =
      IO.eval(test).flatten
  }

  implicit val pure: Compiler[Pure] = new Compiler[Pure] {
    override def compile[A](test: Test[Pure, A]): IO[Test[Pure, A]] =
      IO.pure(test)
  }
}
