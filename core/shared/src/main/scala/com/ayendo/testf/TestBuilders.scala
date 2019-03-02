package com.ayendo.testf

import cats._
import cats.implicits._

trait TestBuilders {
  def cond(predicate: Boolean): Test =
    if (predicate) success else error("false")

  def equal[A: Eq: Show](actual: A, expected: A): Test =
    if (actual === expected) success
    else error(show"$actual does not match expected $expected")

  def group(tests: Test*): Test = Test.Group(tests.toList)

  def label(description: String, test: Test): Test =
    Test.Label(description, test)

  def prefix(description: String, test: Test): Test =
    test match {
      case test: Test.Label => test
      case test             => label(description, test)
    }

  def labelF[F[_]](description: String, test: F[Test])(
      implicit F: ApplicativeError[F, Throwable]): F[Test] =
    test
      .map(Test.label(description, _))
      .handleError(throwable => label(description, failure(throwable)))

  def prefixF[F[_]](description: String, test: F[Test])(
      implicit F: ApplicativeError[F, Throwable]): F[Test] =
    test
      .map {
        case test: Test.Label => test
        case test             => label(description, test)
      }
      .handleError(throwable => label(description, failure(throwable)))

  def liftF[F[_]: Functor, A](value: F[A])(test: A => Test): F[Test] =
    value.map(test)

  def message(description: String, test: Test): Test =
    Test.Message(description, test)

  val success: Test = Test.Success

  def success(description: String): Test = label(description, success)

  def error(message: String): Test = Test.Error(message)

  def failure(throwable: Throwable): Test = Test.Failure(throwable)

  def skip(test: Test): Test = Test.Skip(test)
}
