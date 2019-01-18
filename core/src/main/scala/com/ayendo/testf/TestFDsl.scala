package com.ayendo.testf

import cats._
import cats.implicits._
import sourcecode.Name

trait TestFDsl {
  def assert[F[_]](description: String, result: F[Result]): Test[F] =
    Test.Assert(description, result)

  def condition[F[_]: Functor](predicate: F[Boolean])(
      implicit name: Name): Test[F] =
    condition(name.value, predicate)

  def condition[F[_]: Functor](description: String,
                               predicate: F[Boolean]): Test[F] = {
    val result = predicate.map {
      case true  => Result.Success
      case false => Result.Error("Predicate failed")
    }

    assert(description, result)
  }

  def equal[F[_]: Applicative, A: Eq: Show](description: String,
                                            actual: F[A],
                                            expected: F[A]): Test[F] = {
    val result = (actual, expected).mapN { (actual, expected) =>
      if (actual === expected) Result.Success
      else Result.Error(show"$actual is not equal to $expected")
    }

    assert(description, result)
  }

  def equal[F[_]: Applicative, A: Eq: Show](actual: F[A], expected: F[A])(
      implicit n: Name): Test[F] =
    equal(n.value, actual, expected)

  def fail[F[_]: Applicative](description: String): Test[F] =
    assert(description, (Result.Error("Fail"): Result).pure[F])

  def fail[F[_]: Applicative](implicit name: Name): Test[F] = fail(name.value)

  def label[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Label(description, test)

  def succeed[F[_]: Applicative](description: String): Test[F] =
    assert(description, (Result.Success: Result).pure[F])

  def succeed[F[_]: Applicative](implicit name: Name): Test[F] =
    succeed(name.value)
}
