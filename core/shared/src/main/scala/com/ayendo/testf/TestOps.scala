package com.ayendo.testf

import cats._
import cats.implicits._
import com.ayendo.testf.implicits._
import sbt.testing.Status

final class TestOps[F[_]](test: Test[F]) {
  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Test[G] =
    TestOps.mapK(test, f)

  def compile(implicit F: Monad[F], M: Measure[F]): F[Test.Result] =
    TestOps.compile(test)

  def success(implicit F: Monad[F]): F[Boolean] = TestOps.success(test)

  def error(implicit F: Monad[F]): F[Boolean] = TestOps.error(test)

  def failure(implicit F: Monad[F]): F[Option[Throwable]] =
    TestOps.failure(test)

  def status(implicit F: Monad[F]): F[Status] = TestOps.status(test)
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
      case Test.Success => Test.Success
    }

  def compile[F[_]](
      test: Test[F]
  )(implicit F: Monad[F], M: Measure[F]): F[Test.Result] = test match {
    case test: Test.Defer[F] =>
      M.measure(test.test.flatMap(_.compile)).map {
        case (Test.Result(test, _), duration) =>
          Test.Result(test, duration)
      }
    case test: Test.Error   => Test.Result(test, duration = None).pure[F]
    case test: Test.Failure => Test.Result(test, duration = None).pure[F]
    case test: Test.Group[F] =>
      test.tests.traverse(compile[F]).map { results =>
        val group = Test.Group(results.map(_.test))
        val duration = results.map(_.duration).combineAll
        Test.Result(group, duration)
      }
    case test: Test.Label[F] =>
      compile[F](test.test).map { result =>
        Test.Result(Test.Label(test.description, result.test), result.duration)
      }
    case test: Test.Message[F] =>
      compile[F](test.test).map { result =>
        Test
          .Result(Test.Message(test.description, result.test), result.duration)
      }
    case Test.Success => Test.Result(Test.Success, duration = None).pure[F]
  }

  def success[F[_]: Monad](test: Test[F]): F[Boolean] = test match {
    case test: Test.Defer[F]             => test.test.flatMap(success[F])
    case Test.Error(_) | Test.Failure(_) => false.pure[F]
    case test: Test.Group[F]             => test.tests.forallM(success[F])
    case test: Test.Label[F]             => success(test.test)
    case test: Test.Message[F]           => success(test.test)
    case Test.Success                    => true.pure[F]
  }

  def error[F[_]: Monad](test: Test[F]): F[Boolean] = test match {
    case test: Test.Defer[F]            => test.test.flatMap(error[F])
    case Test.Error(_)                  => true.pure[F]
    case Test.Failure(_) | Test.Success => false.pure[F]
    case test: Test.Group[F]            => test.tests.existsM(error[F])
    case test: Test.Label[F]            => error(test.test)
    case test: Test.Message[F]          => error(test.test)
  }

  def failure[F[_]: Monad](test: Test[F]): F[Option[Throwable]] = test match {
    case test: Test.Defer[F] => test.test.flatMap(failure[F])
    case Test.Error(_) | Test.Success =>
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
    case Test.Success          => Status.Success.pure[F]
  }
}
