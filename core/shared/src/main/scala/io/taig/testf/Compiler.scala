package io.taig.testf

import cats.effect.implicits._
import cats.effect.{Effect, IO}
import cats.implicits._
import cats.{Eval, Functor, Id}
import simulacrum.typeclass

@typeclass
trait Compiler[F[_]] {
  def compile(test: Test[F]): IO[Test[Pure]]
}

object Compiler {
  def default[F[_]: Functor](
      eval: F[IO[Test[Pure]]] => IO[Test[Pure]]
  ): Compiler[F] = new Compiler[F] {
    override def compile(test: Test[F]): IO[Test[Pure]] =
      test match {
        case test: Test.And[F]  => test.tests.traverse(compile).map(Test.and)
        case test: Test.Eval[F] => eval(test.test.map(compile))
        case test: Test.Error   => IO.pure(test)
        case test: Test.Failure => IO.pure(test)
        case test: Test.Label[F] =>
          compile(test.test).map(Test.Label(test.description, _))
        case test: Test.Message[F] =>
          compile(test.test).map(Test.Message(test.description, _))
        case test: Test.Not[F]  => compile(test.test).map(Test.Not.apply)
        case test: Test.Or[F]   => test.tests.traverse(compile).map(Test.or)
        case test: Test.Skip[F] => compile(test.test).map(Test.Skip.apply)
        case Test.Success       => IO.pure(Test.Success)
      }
  }

  implicit val id: Compiler[Id] = default[Id](identity)

  implicit def effect[F[_]: Effect]: Compiler[F] =
    default[F](_.toIO.flatten.handleError(Test.failure))

  implicit val eval: Compiler[Eval] = default[Eval](IO.eval(_).flatten)

  implicit val pure: Compiler[Pure] = IO.pure(_)
}
