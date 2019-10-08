package com.ayendo.testf

import cats.effect.{Effect, IO}
import cats.effect.implicits._
import cats.implicits._
import cats.{Eval, Id}
import simulacrum.typeclass

@typeclass
trait Compiler[F[_]] {
  def compile(test: Test[F]): IO[Test[Pure]]
}

object Compiler {
  implicit val id: Compiler[Id] = new Compiler[Id] {
    override def compile(test: Test[Id]): IO[Test[Pure]] =
      test.fold[Id, IO[Test[Pure]]](
        effect = compile,
        error = message => IO.pure(Test.error(message)),
        failure = throwable => IO.pure(Test.failure(throwable)),
        group = _.traverse(compile).map(Test.group),
        label =
          (description, test) => compile(test).map(Test.label(description, _)),
        message = (description, test) =>
          compile(test).map(Test.message(description, _)),
        success = IO.pure(Test.success)
      )
  }

  implicit def effect[F[_]: Effect]: Compiler[F] = new Compiler[F] {
    override def compile(test: Test[F]): IO[Test[Pure]] =
      test.fold[F, IO[Test[Pure]]](
        effect = _.toIO.flatMap(compile).handleError(Test.failure),
        error = message => IO.pure(Test.error(message)),
        failure = throwable => IO.pure(Test.failure(throwable)),
        group = _.traverse(compile).map(Test.group),
        label =
          (description, test) => compile(test).map(Test.label(description, _)),
        message = (description, test) =>
          compile(test).map(Test.message(description, _)),
        success = IO.pure(Test.success)
      )
  }

  implicit val eval: Compiler[Eval] = new Compiler[Eval] {
    override def compile(test: Test[Eval]): IO[Test[Pure]] =
      test.fold[Eval, IO[Test[Pure]]](
        effect = IO.eval(_).flatMap(compile),
        error = message => IO.pure(Test.error(message)),
        failure = throwable => IO.pure(Test.failure(throwable)),
        group = _.traverse(compile).map(Test.group),
        label =
          (description, test) => compile(test).map(Test.label(description, _)),
        message = (description, test) =>
          compile(test).map(Test.message(description, _)),
        success = IO.pure(Test.success)
      )
  }

  implicit val pure: Compiler[Pure] = new Compiler[Pure] {
    override def compile(test: Test[Pure]): IO[Test[Pure]] = IO.pure(test)
  }
}
