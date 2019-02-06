package com.ayendo.testf

import cats._
import cats.implicits._
import com.ayendo.testf.instance.AllInstances._
import sbt.testing.Status

sealed trait Test[+A] extends Product with Serializable {
  def success: Boolean = this match {
    case Test.Error(_) | Test.Failure(_) => false
    case Test.Group(tests)               => tests.forall(_.success)
    case Test.Label(_, test)             => test.success
    case Test.Message(_, test)           => test.success
    case Test.Success(_) | Test.Skip(_)  => true
  }

  def error: Boolean = this match {
    case Test.Error(_)                                    => true
    case Test.Failure(_) | Test.Success(_) | Test.Skip(_) => false
    case Test.Group(tests)                                => tests.exists(_.error)
    case Test.Label(_, test)                              => test.error
    case Test.Message(_, test)                            => test.error
  }

  def failure: Option[Throwable] = this match {
    case Test.Error(_) | Test.Success(_) | Test.Skip(_) => None
    case Test.Failure(throwable)                        => Some(throwable)
    case Test.Group(tests)                              => tests.collectFirstSome(_.failure)
    case Test.Label(_, test)                            => test.failure
    case Test.Message(_, test)                          => test.failure
  }

  def status: Status = this match {
    case Test.Error(_)         => Status.Error
    case Test.Failure(_)       => Status.Failure
    case Test.Group(tests)     => tests.map(_.status).combineAll
    case Test.Label(_, test)   => test.status
    case Test.Message(_, test) => test.status
    case Test.Skip(_)          => Status.Skipped
    case Test.Success(_)       => Status.Success
  }
}

object Test extends TestBuilders {
  final case class Error(message: String) extends Test[Nothing]

  final case class Failure(throwable: Throwable) extends Test[Nothing]

  final case class Group[A](tests: List[Test[A]]) extends Test[A]

  final case class Label[A](description: String, test: Test[A]) extends Test[A]

  final case class Message[A](description: String, test: Test[A])
      extends Test[A]

  final case class Skip[A](test: Test[A]) extends Test[A]

  final case class Success[A](value: A) extends Test[A]

  implicit val monad: Monad[Test[?]] = new Monad[Test[?]] {
    override def pure[A](x: A): Test[A] = Test.Success(x)

    override def map[A, B](fa: Test[A])(f: A => B): Test[B] =
      fa match {
        case error: Test.Error     => error
        case failure: Test.Failure => failure
        case Test.Group(tests)     => Test.Group(tests.map(map(_)(f)))
        case Test.Label(description, test) =>
          Test.Label(description, map(test)(f))
        case Test.Message(description, test) =>
          Test.Message(description, map(test)(f))
        case Test.Skip(test)     => Test.Skip(map(test)(f))
        case Test.Success(value) => Test.Success(f(value))
      }

    override def flatMap[A, B](fa: Test[A])(f: A => Test[B]): Test[B] =
      fa match {
        case error: Test.Error     => error
        case failure: Test.Failure => failure
        case Test.Group(tests)     => Test.Group(tests.map(flatMap(_)(f)))
        case Test.Label(description, test) =>
          Test.Label(description, flatMap(test)(f))
        case Test.Message(description, test) =>
          Test.Message(description, flatMap(test)(f))
        case Test.Skip(test)     => Test.Skip(flatMap(test)(f))
        case Test.Success(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Test[Either[A, B]]): Test[B] = {
      def go(test: Test[Either[A, B]]): Test[B] = test match {
        case error: Test.Error             => error
        case failure: Test.Failure         => failure
        case Test.Group(tests)             => Test.Group(tests.map(go))
        case Test.Label(description, test) => Test.Label(description, go(test))
        case Test.Message(description, test) =>
          Test.Message(description, go(test))
        case Test.Skip(test)        => Test.Skip(go(test))
        case Test.Success(Right(b)) => Test.Success(b)
        case Test.Success(Left(a))  => go(f(a))
      }

      go(f(a))
    }
  }

  implicit def semigroup[A]: Semigroup[Test[A]] = {
    case (Group(xs), Group(ys)) => Group(xs |+| ys)
    case (Group(xs), y)         => Group(xs :+ y)
    case (x, Group(ys))         => Group(x +: ys)
    case (x, y)                 => Group(List(x, y))
  }

  implicit def eq[A: Eq]: Eq[Test[A]] = new Eq[Test[A]] {
    override def eqv(x: Test[A], y: Test[A]): Boolean = {
      PartialFunction.cond((x, y)) {
        case (Error(m1), Error(m2))           => m1 === m2
        case (Failure(t1), Failure(t2))       => t1 == t2
        case (Group(xs), Group(ys))           => xs === ys
        case (Label(dx, x), Label(dy, y))     => dx === dy && eqv(x, y)
        case (Message(dx, x), Message(dy, y)) => dx === dy && eqv(x, y)
        case (Skip(x), Skip(y))               => eqv(x, y)
        case (Success(x), Success(y))         => x === y
      }
    }
  }

  implicit def show[A]: Show[Test[A]] = new Show[Test[A]] {
    override def show(test: Test[A]): String = test match {
      case Error(message)             => s"Error($message)"
      case Failure(throwable)         => s"Failure(${throwable.getMessage})"
      case Group(tests)               => s"Group(${tests.map(show).mkString(", ")})"
      case Label(description, test)   => s"Label($description, ${show(test)})"
      case Message(description, test) => s"Message($description, ${show(test)})"
      case Skip(test)                 => s"Skip(${show(test)})"
      case Success(value)             => s"Success($value)"
    }
  }
}
