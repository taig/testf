package com.ayendo.testf

import cats._
import cats.implicits._

trait TestBuilders {
  def cond(predicate: Boolean): Test[Nothing] =
    if (predicate) success else error("false")

  def defer[F[_]](test: F[Test[F]]): Test[F] = Test.Defer(test)

  def equal[A: Eq: Show](actual: A, expected: A): Test[Nothing] =
    if (actual === expected) success
    else error(show"$actual does not match expected $expected")

  def group[F[_]](tests: Test[F]*): Test[F] = Test.Group(tests.toList)

  def label[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Label(description, test)

  def message[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Message(description, test)

  def prefix[F[_]](description: String, test: Test[F]): Test[F] =
    test match {
      case test: Test.Label[F] => test
      case test                => label(description, test)
    }

  val success: Test[Nothing] = Test.Success

  def success(description: String): Test[Nothing] = label(description, success)

  def error(message: String): Test[Nothing] = Test.Error(message)

  def failure(throwable: Throwable): Test[Nothing] = Test.Failure(throwable)
}
