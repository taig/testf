package com.ayendo.testf

import cats.data.Validated
import cats.implicits._
import cats.{Eq, Functor, Monad, Monoid, Show}
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

      Suspend(value)
    }

  def notEqualF(
      expected: F[A])(implicit F: Monad[F], E: Eq[A], S: Show[A]): Assert[F] =
    test.flatMap { actual =>
      val value: F[Test[F, Assertion]] = expected.map { expected =>
        if (actual =!= expected) Success[F, Assertion]()
        else Error(show"$actual does match expected $expected")
      }

      Suspend(value)
    }
}

class TestOpsAssertion[F[_]](val test: Test[F, Assertion]) extends AnyVal {
  def compile(implicit F: Monad[F]): F[Report] = TestOpsAssertion.compile(None, test)
}

object TestOpsAssertion {
  private def compile[F[_]](description: Option[String], test: Test[F, Assertion])(
      implicit F: Monad[F]): F[Report] =
    (description, test) match {
      case (description, Error(message)) =>
        F.pure(Report.Error(description.getOrElse("error"), message))
      case (description, Failure(throwable)) =>
        F.pure(Report.Failure(description.getOrElse("failure"), throwable))
      case (description, Group(tests)) =>
        tests.traverse(compile(None, _)).map(Report.Group(_, description))
      case (d1, label: Label[F, Assertion]) =>
        compile(d1.orElse(Some(label.description)), label.test)
      case (description, Pure(_)) =>
        F.pure(Report.Success(description.getOrElse("pure")))
      case (description, Skip(test)) =>
        F.pure(Report.Skip(description.getOrElse("skip")))
      case (description, Success()) =>
        F.pure(Report.Success(description.getOrElse("success")))
      case (description, Suspend(test)) => test.flatMap(compile(description, _))
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

final class TestOpsMonoid[F[_], A](val test: Test[F, A]) {
  def isEmpty(implicit F: Functor[F],
              M: Monoid[A],
              E: Eq[A],
              S: Show[A]): Assert[F] = test.flatMap { value =>
    if (value.isEmpty) Success()
    else Error(show"not empty $value")
  }

  def nonEmpty(implicit F: Functor[F],
               M: Monoid[A],
               E: Eq[A],
               S: Show[A]): Assert[F] = test.flatMap { value =>
    if (value.isEmpty) Error(show"empty $value") else Success()
  }
}

final class TestOpsValidated[F[_], A, B](val test: Test[F, Validated[A, B]]) {
  def isValid(implicit F: Functor[F], S: Show[A]): Assert[F] = test.flatMap {
    validated =>
      validated.fold(value => Error(show"invalid $value"), _ => Success())
  }

  def isInvalid(implicit F: Functor[F], S: Show[B]): Assert[F] = test.flatMap {
    validated =>
      validated.fold(_ => Success(), value => Error(show"valid $value"))
  }
}
