package com.ayendo.testf

import cats._
import cats.implicits._
import sourcecode.Name

trait TestFDsl {
  def assert[F[_]: Lift](description: String, result: F[Result]): Test =
    Test.Assert(description, Lift[F].toIO(result))

  def condition[F[_]: Functor: Lift](predicate: F[Boolean])(
      implicit name: Name): Test =
    condition(name.value, predicate)

  def condition[F[_]: Functor: Lift](description: String,
                                     predicate: F[Boolean]): Test = {
    val result = predicate.map {
      case true  => Result.Success
      case false => Result.Error("Predicate failed")
    }

    assert(description, result)
  }

  def equal[F[_]: Functor: Semigroupal: Lift, A: Eq: Show](
      description: String,
      actual: F[A],
      expected: F[A]): Test = {
    val result = (actual, expected).mapN { (actual, expected) =>
      if (actual === expected) Result.Success
      else Result.Error(show"$actual is not equal to $expected")
    }

    assert(description, result)
  }

  def equal[F[_]: Functor: Semigroupal: Lift, A: Eq: Show](
      actual: F[A],
      expected: F[A])(implicit n: Name): Test =
    equal(n.value, actual, expected)

  def fail[F[_]: Applicative: Lift](description: String): Test =
    assert(description, (Result.Error("Fail"): Result).pure[F])

  def fail[F[_]: Applicative: Lift](implicit name: Name): Test =
    fail[F](name.value)

  def label(description: String, test: Test): Test =
    Test.Label(description, test)

  def succeed[F[_]: Applicative: Lift](description: String): Test =
    assert(description, (Result.Success: Result).pure[F])

  def succeed[F[_]: Applicative: Lift](implicit name: Name): Test =
    succeed[F](name.value)
}
