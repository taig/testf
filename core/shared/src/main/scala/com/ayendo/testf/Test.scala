package com.ayendo.testf

import cats._
import cats.effect.implicits._
import cats.data.Validated
import cats.effect.{Effect, IO}
import cats.implicits._

sealed trait Test[+A] extends Product with Serializable

object Test {
  final case class Effectful[A](test: IO[Test[A]]) extends Test[A]

  final case class Error(message: String) extends Test[Nothing]

  final case class Failure(throwable: Throwable) extends Test[Nothing]

  final case class Group[A](left: Test[A], right: Test[A]) extends Test[A]

  final case class Label[A](description: String, test: Test[A]) extends Test[A]

  final case class Pure[A](value: A) extends Test[A]

  final case class Skip[A](test: Test[A]) extends Test[A]

  final case object Success extends Test[Nothing]

  def apply[A](description: String, value: A): Test[A] =
    pure(description, value)

  def effect[F[_]: Effect, A](description: String, value: F[A]): Test[A] =
    label(description, Effectful(value.toIO.map(Pure.apply)))

  def label[A](description: String, test: Test[A]): Test[A] =
    Label(description, test)

  def pure[A](description: String, value: A): Test[A] =
    label(description, Pure(value))

  def success(description: String): Test[Nothing] = label(description, Success)

  def error(description: String, message: String): Test[Nothing] =
    label(description, Error(message))

  def failure(description: String, throwable: Throwable): Test[Nothing] =
    label(description, Failure(throwable))

  implicit val monad: Monad[Test] = new Monad[Test] {
    override def flatMap[A, B](fa: Test[A])(f: A => Test[B]): Test[B] =
      fa match {
        case Effectful(test) =>
          val result: IO[Test[B]] = test.flatMap {
            case Effectful(test)  => test.map(flatMap(_)(f))
            case error: Error     => IO.pure(error)
            case failure: Failure => IO.pure(failure)
            case Group(left, right) =>
              IO.pure(Group(flatMap(left)(f), flatMap(right)(f)))
            case Label(description, test) =>
              IO.pure(Label(description, flatMap(test)(f)))
            case Pure(value) => IO.pure(f(value))
            case Skip(test)  => IO.pure(flatMap(test)(f))
            case Success     => IO.pure(Success)
          }

          Effectful(result)
        case error: Error             => error
        case failure: Failure         => failure
        case Group(left, right)       => Group(flatMap(left)(f), flatMap(right)(f))
        case Label(description, test) => Label(description, flatMap(test)(f))
        case Pure(value)              => f(value)
        case Skip(test)               => Skip(flatMap(test)(f))
        case Success                  => Success
      }

    override def tailRecM[A, B](a: A)(f: A => Test[Either[A, B]]): Test[B] = ???

    override def pure[A](x: A): Test[A] = Pure(x)
  }

  implicit def semigroup[A]: Semigroup[Test[A]] = new Semigroup[Test[A]] {
    override def combine(x: Test[A], y: Test[A]): Test[A] = Group(x, y)
  }

  implicit class TestOps[A](val test: Test[A]) extends AnyVal {
    def equal(expected: A)(implicit eq: Eq[A]): Assert =
      test.map { actual =>
        actual === expected
      }.isTrue

    def notEqual(expected: A)(implicit eq: Eq[A]): Assert =
      test.map { actual =>
        actual =!= expected
      }.isTrue

    def equalF[F[_]: Effect](expected: F[A])(implicit eq: Eq[A]): Assert =
      test.flatMap { actual =>
        Effectful((actual.pure[F], expected).mapN(_ === _).toIO.map(Pure.apply))
      }.isTrue
  }

  implicit class TestBooleanOps(val test: Test[Boolean]) extends AnyVal {
    def isTrue: Assert = test.flatMap {
      case true  => Success
      case false => Error("false")
    }

    def isFalse: Assert = test.flatMap {
      case false => Success
      case true  => Error("true")
    }
  }

  implicit class TestValidatedOps[A, B](val test: Test[Validated[A, B]])
      extends AnyVal {
    def isValid: Assert = test.flatMap { validated =>
      if (validated.isValid) Success
      else Error("invalid")
    }

    def isInvalid: Assert = test.flatMap { validated =>
      if (validated.isInvalid) Success
      else Error("valid")
    }
  }

  implicit class TestOpsResult(val test: Assert) extends AnyVal {
    def run: IO[Summary] = Test.run(None, test)
  }

  private val run: (Option[String], Assert) => IO[Summary] = {
    case (description, Effectful(test)) => test.flatMap(run(description, _))
    case (description, Error(message)) =>
      IO.pure(Summary.Error(description.getOrElse("error"), message))
    case (description, Failure(throwable)) =>
      IO.pure(Summary.Failure(description.getOrElse("failure"), throwable))
    case (description, Group(left, right)) =>
      (run(None, left), run(None, right)).mapN(Summary.Group(_, _, description))
    case (d1, Label(d2, test)) => run(d1.orElse(Some(d2)), test)
    case (description, Pure(_)) =>
      IO.pure(Summary.Success(description.getOrElse("pure")))
    case (_, Skip(_)) => ???
    case (description, Success) =>
      IO.pure(Summary.Success(description.getOrElse("success")))
  }
}
