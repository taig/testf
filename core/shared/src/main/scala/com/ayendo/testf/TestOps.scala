package com.ayendo.testf

import cats.data.Validated
import cats.implicits._
import cats.{Eq, Functor, Monad, Monoid, Show}
import com.ayendo.testf.Test._

final class TestOps[F[_], A](val test: Test[F, A]) extends AnyVal {
  def equal(expected: A)(implicit F: Functor[F],
                         E: Eq[A],
                         S: Show[A]): Test[F, Assertion] =
    test.flatMap { actual =>
      if (actual === expected) Success[F, Assertion]()
      else Error(show"$actual does not match expected $expected")
    }

  def notEqual(expected: A)(implicit F: Functor[F],
                            E: Eq[A],
                            S: Show[A]): Test[F, Assertion] =
    test.flatMap { actual =>
      if (actual =!= expected) Success[F, Assertion]()
      else Error(show"$actual does match expected $expected")
    }

  def equalF(expected: F[A])(implicit F: Monad[F],
                             E: Eq[A],
                             S: Show[A]): Test[F, Assertion] =
    test.flatMap { actual =>
      val value: F[Test[F, Assertion]] = expected.map { expected =>
        if (actual === expected) Success[F, Assertion]()
        else Error(show"$actual does not match expected $expected")
      }

      Suspend(value)
    }

  def notEqualF(expected: F[A])(implicit F: Monad[F],
                                E: Eq[A],
                                S: Show[A]): Test[F, Assertion] =
    test.flatMap { actual =>
      val value: F[Test[F, Assertion]] = expected.map { expected =>
        if (actual =!= expected) Success[F, Assertion]()
        else Error(show"$actual does match expected $expected")
      }

      Suspend(value)
    }
}

class TestOpsAssertion[F[_]](val test: Test[F, Assertion]) extends AnyVal {
  def compile(implicit F: Monad[F]): F[Report] = test match {
    case Error(message)     => F.pure(Report.Error("error", message))
    case Failure(throwable) => F.pure(Report.Failure("failure", throwable))
    case Group(tests) =>
      tests.traverse(_.compile).map(Report.Group(_, description = None))
    case Label(description, test) =>
      test.compile.map(_.withDescription(description))
    case Pure(_)       => F.pure(Report.Success("pure"))
    case Skip(_)       => F.pure(Report.Skip("skip"))
    case Success()     => F.pure(Report.Success("success"))
    case Suspend(test) => test.flatMap(_.compile)
  }
}

final class TestOpsBoolean[F[_]](val test: Test[F, Boolean]) extends AnyVal {
  def isTrue(implicit F: Functor[F]): Test[F, Assertion] = test.flatMap {
    case true  => Success()
    case false => Error("false")
  }

  def isFalse(implicit F: Functor[F]): Test[F, Assertion] = test.flatMap {
    case true  => Error("true")
    case false => Success()
  }
}

final class TestOpsMonoid[F[_], A](val test: Test[F, A]) extends AnyVal {
  def isEmpty(implicit F: Functor[F],
              M: Monoid[A],
              E: Eq[A],
              S: Show[A]): Test[F, Assertion] = test.flatMap { value =>
    if (value.isEmpty) Success()
    else Error(show"not empty $value")
  }

  def nonEmpty(implicit F: Functor[F],
               M: Monoid[A],
               E: Eq[A],
               S: Show[A]): Test[F, Assertion] = test.flatMap { value =>
    if (value.isEmpty) Error(show"empty $value") else Success()
  }
}

final class TestOpsValidated[F[_], A, B](val test: Test[F, Validated[A, B]]) extends AnyVal {
  def isValid(implicit F: Functor[F], S: Show[A]): Test[F, Assertion] =
    test.flatMap { validated =>
      validated.fold(value => Error(show"invalid $value"), _ => Success())
    }

  def isInvalid(implicit F: Functor[F], S: Show[B]): Test[F, Assertion] =
    test.flatMap { validated =>
      validated.fold(_ => Success(), value => Error(show"valid $value"))
    }
}
