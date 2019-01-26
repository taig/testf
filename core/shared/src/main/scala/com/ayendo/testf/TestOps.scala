package com.ayendo.testf

import cats.data.Validated
import cats.implicits._
import cats.{Eq, Functor, Monad, Show}
import com.ayendo.testf.Test._

final class TestOps[F[_], A](val test: Test[F, A]) extends AnyVal {
  def equal(
      expected: A)(implicit F: Functor[F], E: Eq[A], S: Show[A]): Assert[F] =
    test.flatMap { actual =>
      if (actual === expected) Success[F, Assertion]()
      else Error(show"$actual does not match expected $expected")
    }

  def notEqual(
      expected: A)(implicit F: Functor[F], E: Eq[A], S: Show[A]): Assert[F] =
    test.flatMap { actual =>
      if (actual =!= expected) Success[F, Assertion]()
      else Error(show"$actual does match expected $expected")
    }

  def equalF(
      expected: F[A])(implicit F: Monad[F], E: Eq[A], S: Show[A]): Assert[F] =
    test.flatMap { actual =>
      val value: F[Test[F, Assertion]] = expected.map { expected =>
        if (actual === expected) Success[F, Assertion]()
        else Error(show"$actual does not match expected $expected")
      }

      Defer(value)
    }

  def notEqualF(
      expected: F[A])(implicit F: Monad[F], E: Eq[A], S: Show[A]): Assert[F] =
    test.flatMap { actual =>
      val value: F[Test[F, Assertion]] = expected.map { expected =>
        if (actual =!= expected) Success[F, Assertion]()
        else Error(show"$actual does match expected $expected")
      }

      Defer(value)
    }
}

final class TestOpsBoolean[F[_]](val test: Test[F, Boolean]) extends AnyVal {
  def isTrue(implicit F: Functor[F]): Assert[F] = test.flatMap {
    case true  => Success()
    case false => Error("false")
  }

  def isFalse(implicit F: Functor[F]): Assert[F] = test.flatMap {
    case true  => Error("true")
    case false => Success()
  }
}

final class TestOpsEither[F[_], A, B](val test: Test[F, Either[A, B]])
    extends AnyVal {}

final class TestOpsOption[F[_], A](val test: Test[F, Option[A]])
    extends AnyVal {
  def isDefined(implicit F: Functor[F]): Assert[F] =
    test.flatMap { option =>
      option.fold[Assert[F]](Error("Option is empty"))(_ => Success())
    }

  def isEmpty(implicit F: Functor[F], S: Show[A]): Assert[F] =
    test.flatMap { option =>
      option.fold[Assert[F]](Success()) { value =>
        Error(show"Option is not empty: $value")
      }
    }
}

final class TestOpsValidated[F[_], A, B](val test: Test[F, Validated[A, B]])
    extends AnyVal {}
