package com.ayendo.testf

import cats._
import cats.implicits._

trait TestBuilders {
  def apply[F[_], A](label: String, value: A): Test[F, A] =
    this.success(label, value)

  def cond[F[_]](predicate: Boolean): Test[F, Unit] =
    if (predicate) this.unit else this.error("false")

  def cond[F[_]](label: String, predicate: Boolean): Test[F, Unit] =
    this.label(label, this.cond(predicate))

  def equal[F[_], A: Eq: Show](actual: A, expected: A): Test[F, Unit] =
    if (actual === expected) this.unit
    else this.error(show"$actual does not match expected $expected")

  def equal[F[_], A: Eq: Show](label: String,
                               actual: A,
                               expected: A): Test[F, Unit] =
    this.label(label, this.equal(actual, expected))

  def liftF[F[_]: Functor, A](value: F[A]): Test[F, A] =
    Test.Defer(value.map(success[F, A]))

  def liftF[F[_]: Functor, A](label: String, value: F[A]): Test[F, A] =
    this.label(label, Test.Defer(value.map(success[F, A])))

  def group[F[_], A](tests: Test[F, A]*): Test[F, A] = Test.Group(tests.toList)

  def label[F[_], A](label: String, test: Test[F, A]): Test[F, A] =
    Test.Label(label, test)

  def message[F[_], A](label: String, test: Test[F, A]): Test[F, A] =
    Test.Message(label, test)

  def pure[F[_], A](value: A): Test[F, A] = Test.Success(value)

  def success[F[_], A](value: A): Test[F, A] = Test.Success(value)

  def success[F[_], A](label: String, value: A): Test[F, A] =
    this.label(label, this.success(value))

  def error[F[_]](message: String): Test[F, Nothing] = Test.Error(message)

  def error[F[_]](label: String, message: String): Test[F, Nothing] =
    this.label(label, this.error(message))

  def failure[F[_]](label: String, throwable: Throwable): Test[F, Nothing] =
    this.label(label, Test.Failure(throwable))

  def skip[F[_], A](test: Test[F, A]): Test[F, A] = Test.Skip(test)

  def unit[F[_]]: Test[F, Unit] = success(())

  def unit[F[_]](label: String): Test[F, Unit] = this.success(label, ())
}
