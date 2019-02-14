package com.ayendo.testf

import cats.{Id, Monad}
import cats.implicits._

final class TestOps[F[_], A](val test: Test[F, A]) extends AnyVal {
  def compile(implicit F: Monad[F]): F[Test[Id, A]] = TestOps.compile(test)
}

object TestOps {
  def compile[F[_]: Monad, A](test: Test[F, A]): F[Test[Id, A]] = test match {
    case Test.Defer(test)         => test.flatMap(compile(_))
    case error: Test.Error[F]     => error.asInstanceOf[Test[Id, A]].pure[F]
    case failure: Test.Failure[F] => failure.asInstanceOf[Test[Id, A]].pure[F]
    case Test.Group(tests)        => tests.traverse(compile(_)).map(Test.Group.apply)
    case Test.Label(description, test) =>
      compile(test).map(Test.Label(description, _))
    case Test.Message(description, test) =>
      compile(test).map(Test.Message(description, _))
    case Test.Skip(test) => compile(test).map(Test.Skip.apply)
    case success: Test.Success[F, A] =>
      success.asInstanceOf[Test[Id, A]].pure[F]
  }
}
