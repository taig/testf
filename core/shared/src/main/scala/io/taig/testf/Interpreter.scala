package io.taig.testf

import cats._
import cats.effect.IO
import cats.implicits._

trait Interpreter[F[_], G[_]] {
  def interpret[A](test: Test[F, A]): G[Test[Pure, A]]
}

object Interpreter extends Interpreter1 {
  implicit def effect[F[_]](
      implicit F: MonadError[F, Throwable],
      P: Parallel[F]
  ): Interpreter[F, F] =
    new Interpreter[F, F] {
      override def interpret[A](test: Test[F, A]): F[Test[Pure, A]] =
        test match {
          case test: Test.And[F, A] =>
            test.tests.parTraverse(interpret[A]).map(Test.and)
          case test: Test.Eval[F, A] =>
            test.test.flatMap(interpret[A]).handleError(Test.failure)
          case test: Test.Error   => test.pure[F].widen
          case test: Test.Failure => test.pure[F].widen
          case test: Test.Label[F, A] =>
            interpret(test.test).map(Test.Label(test.description, _))
          case test: Test.Message[F, A] =>
            interpret(test.test).map(Test.Message(test.description, _))
          case test: Test.Not[F, A] => interpret(test.test).map(Test.not)
          case test: Test.Or[F, A] =>
            test.tests.parTraverse(interpret[A]).map(Test.or)
          case test: Test.Skip[F, A] => interpret(test.test).map(Test.skip)
          case test: Test.Success[A] => test.pure[F].widen
        }
    }

  implicit val pureId: Interpreter[Pure, Id] = new Interpreter[Pure, Id] {
    override def interpret[A](test: Test[Pure, A]): Id[Test[Pure, A]] = test
  }

  implicit val pureIO: Interpreter[Pure, IO] = new Interpreter[Pure, IO] {
    override def interpret[A](test: Test[Pure, A]): IO[Test[Pure, A]] =
      IO.pure(test)
  }
}

trait Interpreter1 {
  implicit def sequential[F[_]](
      implicit F: MonadError[F, Throwable]
  ): Interpreter[F, F] =
    new Interpreter[F, F] {
      override def interpret[A](test: Test[F, A]): F[Test[Pure, A]] =
        test match {
          case test: Test.And[F, A] =>
            test.tests.traverse(interpret[A]).map(Test.and)
          case test: Test.Eval[F, A] =>
            test.test.flatMap(interpret[A]).handleError(Test.failure)
          case test: Test.Error   => test.pure[F].widen
          case test: Test.Failure => test.pure[F].widen
          case test: Test.Label[F, A] =>
            interpret(test.test).map(Test.Label(test.description, _))
          case test: Test.Message[F, A] =>
            interpret(test.test).map(Test.Message(test.description, _))
          case test: Test.Not[F, A] => interpret(test.test).map(Test.not)
          case test: Test.Or[F, A] =>
            test.tests.traverse(interpret[A]).map(Test.or)
          case test: Test.Skip[F, A] => interpret(test.test).map(Test.skip)
          case test: Test.Success[A] => test.pure[F].widen
        }
    }
}
