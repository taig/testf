package com.ayendo.testf

import cats.{Functor, Id}
import cats.implicits._

trait TestBuilders {
  def defer[F[_]: Functor, A](description: String, value: F[A]): Test[F, A] =
    label(description, Test.Suspend[F, A](value.map(Test.Success.apply)))

  def label[F[_], A](description: String, test: Test[F, A]): Test[F, A] =
    Test.Label(description, test)

  def pure[A](description: String, value: A): Test[Id, A] =
    success(description, value)

  def success[F[_], A](description: String, value: A): Test[F, A] =
    label(description, Test.Success(value))

  def error[F[_]](description: String, message: String): Test[F, Nothing] =
    label(description, Test.Error(message))

  def failure[F[_]](description: String,
                    throwable: Throwable): Test[F, Nothing] =
    label(description, Test.Failure(throwable))

  def result[F[_]](report: Report): Test[F, Nothing] = Test.Result(report)

  def skip[F[_], A](test: Test[F, A]): Test[F, A] = Test.Skip(test)

  def suspend[F[_], A](test: F[Test[F, A]]): Test[F, A] = Test.Suspend(test)
}
