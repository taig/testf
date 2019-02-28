package com.ayendo.testf

import cats._
import cats.implicits._

trait TestBuilders {
  def cond(predicate: Boolean): Test =
    if (predicate) this.success else this.error("false")

  def cond(description: String, predicate: Boolean): Test =
    this.label(description, this.cond(predicate))

  def equal[A: Eq: Show](actual: A, expected: A): Test =
    if (actual === expected) this.success
    else this.error(show"$actual does not match expected $expected")

  def equal[A: Eq: Show](description: String, actual: A, expected: A): Test =
    this.label(description, this.equal(actual, expected))

  def group(tests: Test*): Test = Test.Group(tests.toList)

  def label(description: String, test: Test): Test =
    Test.Label(description, test)

  def labelF[F[_]: ApplicativeError[?[_], Throwable]](description: String,
                                                      test: F[Test]): F[Test] =
    test
      .map(Test.label(description, _))
      .handleError(Test.failure(description, _))

  def liftF[F[_]: Functor, A](value: F[A])(test: A => Test): F[Test] =
    value.map(test)

  def message(description: String, test: Test): Test =
    Test.Message(description, test)

  val success: Test = Test.Success

  def success(description: String): Test = this.label(description, this.success)

  def error(message: String): Test = Test.Error(message)

  def error(description: String, message: String): Test =
    this.label(description, this.error(message))

  def failure(throwable: Throwable): Test = Test.Failure(throwable)

  def failure(description: String, throwable: Throwable): Test =
    this.label(description, Test.failure(throwable))

  def skip(test: Test): Test = Test.Skip(test)
}
