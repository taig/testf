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

  def labelF[F[_]: ApplicativeError[?[_], Throwable]](description: String,
                                                      test: F[Test]): F[Test] =
    test
      .map(Test.label(description, _))
      .handleError(throwable => label(description, Test.failure(throwable)))

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
