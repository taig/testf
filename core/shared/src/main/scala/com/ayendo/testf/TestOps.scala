package com.ayendo.testf

import cats._
import cats.implicits._
import com.ayendo.testf.implicits._
import sbt.testing.Status

final class TestOps[F[_]: Monad](test: Test[F]) {
  def mapK[G[_]](f: F ~> G): Test[G] = TestOps.mapK(test, f)

  def compile: F[Test[Id]] = TestOps.compile(test)

  def success: F[Boolean] = TestOps.success(test)

  def error: F[Boolean] = TestOps.error(test)

  def failure: F[Option[Throwable]] = TestOps.failure(test)

  def status: F[Status] = TestOps.status(test)
}

object TestOps {
  def mapK[F[_]: Functor, G[_]](test: Test[F], f: F ~> G): Test[G] =
    test match {
      case test: Test.Defer[F] => Test.Defer(f.apply(test.test.map(mapK(_, f))))
      case test: Test.Error    => test
      case test: Test.Failure  => test
      case test: Test.Group[F] => Test.Group(test.tests.map(mapK(_, f)))
      case test: Test.Label[F] =>
        Test.Label(test.description, mapK(test.test, f))
      case test: Test.Message[F] =>
        Test.Message(test.description, mapK(test.test, f))
      case test: Test.Skip[F] => Test.Skip(mapK(test.test, f))
      case Test.Success       => Test.Success
    }

  def compile[F[_]: Monad](test: Test[F]): F[Test[Id]] = test match {
    case test: Test.Defer[F] => test.test.flatMap(compile[F])
    case test: Test.Error    => test.pure[F].widen[Test[Id]]
    case test: Test.Failure  => test.pure[F].widen[Test[Id]]
    case test: Test.Group[F] =>
      test.tests.traverse(compile[F]).map(Test.Group.apply)
    case test: Test.Label[F] =>
      compile[F](test.test).map(Test.Label(test.description, _))
    case test: Test.Message[F] =>
      compile[F](test.test).map(Test.Message(test.description, _))
    case test: Test.Skip[F] => compile[F](test.test).map(Test.Skip.apply)
    case Test.Success       => Test.Success.pure[F].widen[Test[Id]]
  }

  def success[F[_]: Monad](test: Test[F]): F[Boolean] = test match {
    case test: Test.Defer[F]             => test.test.flatMap(success[F])
    case Test.Error(_) | Test.Failure(_) => false.pure[F]
    case test: Test.Group[F]             => test.tests.forallM(success[F])
    case test: Test.Label[F]             => success(test.test)
    case test: Test.Message[F]           => success(test.test)
    case Test.Success | _: Test.Skip[F]  => true.pure[F]
  }

  def error[F[_]: Monad](test: Test[F]): F[Boolean] = test match {
    case test: Test.Defer[F]                              => test.test.flatMap(error[F])
    case Test.Error(_)                                    => true.pure[F]
    case Test.Failure(_) | Test.Success | _: Test.Skip[F] => false.pure[F]
    case test: Test.Group[F]                              => test.tests.existsM(error[F])
    case test: Test.Label[F]                              => error(test.test)
    case test: Test.Message[F]                            => error(test.test)
  }

  def failure[F[_]: Monad](test: Test[F]): F[Option[Throwable]] = test match {
    case test: Test.Defer[F] => test.test.flatMap(failure[F])
    case Test.Error(_) | Test.Success | _: Test.Skip[F] =>
      None.pure[F].widen[Option[Throwable]]
    case Test.Failure(throwable) =>
      Some(throwable).pure[F].widen[Option[Throwable]]
    case test: Test.Group[F]   => test.tests.collectFirstSomeM(failure[F])
    case test: Test.Label[F]   => failure(test.test)
    case test: Test.Message[F] => failure(test.test)
  }

  def status[F[_]: Monad](test: Test[F]): F[Status] = test match {
    case test: Test.Defer[F]   => test.test.flatMap(status[F])
    case Test.Error(_)         => Status.Error.pure[F]
    case Test.Failure(_)       => Status.Failure.pure[F]
    case test: Test.Group[F]   => test.tests.traverse(status[F]).map(_.combineAll)
    case test: Test.Label[F]   => status(test.test)
    case test: Test.Message[F] => status(test.test)
    case _: Test.Skip[F]       => Status.Skipped.pure[F]
    case Test.Success          => Status.Success.pure[F]
  }
}
