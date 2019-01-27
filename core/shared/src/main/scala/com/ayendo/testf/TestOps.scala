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
  def run(implicit F: Monad[F]): F[Summary] = TestOpsAssertion.run(None, test)
}

object TestOpsAssertion {
  private def run[F[_]](description: Option[String], test: Test[F, Assertion])(
      implicit F: Monad[F]): F[Summary] =
    (description, test) match {
      case (description, Error(message)) =>
        F.pure(Summary.Error(description.getOrElse("error"), message))
      case (description, Failure(throwable)) =>
        F.pure(Summary.Failure(description.getOrElse("failure"), throwable))
      case (description, Group(tests)) =>
        tests.traverse(run(None, _)).map(Summary.Group(_, description))
      case (d1, label: Label[F, Assertion]) =>
        run(d1.orElse(Some(label.description)), label.test)
      case (description, Pure(_)) =>
        F.pure(Summary.Success(description.getOrElse("pure")))
      case (description, Skip(test)) =>
        F.pure(Summary.Skip(description.getOrElse("skip")))
      case (description, Success()) =>
        F.pure(Summary.Success(description.getOrElse("success")))
      case (description, Suspend(test)) => test.flatMap(run(description, _))
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
