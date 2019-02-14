package com.ayendo.testf

import cats._
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.Test.Defer
import com.ayendo.testf.instance.AllInstances._
import sbt.testing.Status

sealed trait Test[F[_], +A] extends Product with Serializable {
  def mapK[G[_]](f: F ~> G)(implicit F: Monad[F]): Test[G, A] = this match {
    case Test.Defer(test)              => Test.Defer(f(test.map(_.mapK(f))))
    case error: Test.Error[F]          => error.asInstanceOf[Test[G, A]]
    case failure: Test.Failure[F]      => failure.asInstanceOf[Test[G, A]]
    case Test.Group(tests)             => Test.Group(tests.map(_.mapK(f)))
    case Test.Label(description, test) => Test.Label(description, test.mapK(f))
    case Test.Message(description, test) =>
      Test.Message(description, test.mapK(f))
    case Test.Skip(test)             => Test.Skip(test.mapK(f))
    case success: Test.Success[F, A] => success.asInstanceOf[Test[G, A]]
  }

  def success(implicit F: Monad[F]): F[Boolean] = this match {
    case Test.Defer(test)                => test.flatMap(_.success)
    case Test.Error(_) | Test.Failure(_) => false.pure[F]
    case Test.Group(tests) =>
      tests.traverse(_.success).map(_.forall(identity))
    case Test.Label(_, test)            => test.success
    case Test.Message(_, test)          => test.success
    case Test.Success(_) | Test.Skip(_) => true.pure[F]
  }

  def error(implicit F: Monad[F]): F[Boolean] = this match {
    case Test.Defer(test)                                 => test.flatMap(_.error)
    case Test.Error(_)                                    => true.pure[F]
    case Test.Failure(_) | Test.Success(_) | Test.Skip(_) => false.pure[F]
    case Test.Group(tests)                                => tests.existsM(_.error)
    case Test.Label(_, test)                              => test.error
    case Test.Message(_, test)                            => test.error
  }

  def failure(implicit F: Monad[F]): F[Option[Throwable]] = this match {
    case Test.Defer(test) => test.flatMap(_.failure)
    case Test.Error(_) | Test.Success(_) | Test.Skip(_) =>
      None.pure[F].widen[Option[Throwable]]
    case Test.Failure(throwable) =>
      Some(throwable).pure[F].widen[Option[Throwable]]
    case Test.Group(tests)     => tests.collectFirstSomeM(_.failure)
    case Test.Label(_, test)   => test.failure
    case Test.Message(_, test) => test.failure
  }

  def status(implicit F: Monad[F]): F[Status] = this match {
    case Test.Defer(test)      => test.flatMap(_.status)
    case Test.Error(_)         => Status.Error.pure[F]
    case Test.Failure(_)       => Status.Failure.pure[F]
    case Test.Group(tests)     => tests.traverse(_.status).map(_.combineAll)
    case Test.Label(_, test)   => test.status
    case Test.Message(_, test) => test.status
    case Test.Skip(_)          => Status.Skipped.pure[F]
    case Test.Success(_)       => Status.Success.pure[F]
  }
}

object Test extends TestBuilders with Test1 {
  final case class Defer[F[_], A](test: F[Test[F, A]]) extends Test[F, A]

  final case class Error[F[_]](message: String) extends Test[F, Nothing]

  final case class Failure[F[_]](throwable: Throwable) extends Test[F, Nothing]

  final case class Group[F[_], A](tests: List[Test[F, A]]) extends Test[F, A]

  final case class Label[F[_], A](description: String, test: Test[F, A])
      extends Test[F, A]

  final case class Message[F[_], A](description: String, test: Test[F, A])
      extends Test[F, A]

  final case class Skip[F[_], A](test: Test[F, A]) extends Test[F, A]

  final case class Success[F[_], A](value: A) extends Test[F, A]

  implicit val monadId: Monad[Test[Id, ?]] = monad[Id]

  def liftId: Id ~> IO = new (Id ~> IO) {
    override def apply[A](fa: Id[A]): IO[A] = IO.pure(fa)
  }

  def liftEval: Eval ~> IO = new (Eval ~> IO) {
    override def apply[A](fa: Eval[A]): IO[A] = IO.eval(fa)
  }

  implicit def semigroup[F[_], A]: Semigroup[Test[F, A]] = {
    case (Group(xs), Group(ys)) => Group(xs |+| ys)
    case (Group(xs), y)         => Group(xs :+ y)
    case (x, Group(ys))         => Group(x +: ys)
    case (x, y)                 => Group(List(x, y))
  }

  implicit def eq[A: Eq]: Eq[Test[Id, A]] =
    new Eq[Test[Id, A]] {
      override def eqv(x: Test[Id, A], y: Test[Id, A]): Boolean = {
        PartialFunction.cond((x, y)) {
          case (Defer(x), Defer(y))             => eqv(x, y)
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

  implicit def show[F[_], A]: Show[Test[F, A]] =
    new Show[Test[F, A]] {
      override def show(test: Test[F, A]): String = test match {
        case Defer(test)              => s"Defer($test)"
        case Error(message)           => s"Error($message)"
        case Failure(throwable)       => s"Failure(${throwable.getMessage})"
        case Group(tests)             => s"Group(${tests.map(show).mkString(", ")})"
        case Label(description, test) => s"Label($description, ${show(test)})"
        case Message(description, test) =>
          s"Message($description, ${show(test)})"
        case Skip(test)     => s"Skip(${show(test)})"
        case Success(value) => s"Success($value)"
      }
    }

  implicit def testOpsSyntax[F[_], A](test: Test[F, A]): TestOps[F, A] =
    new TestOps[F, A](test)
}

trait Test1 {
  implicit def monad[F[_]: Functor]: Monad[Test[F, ?]] =
    new Monad[Test[F, ?]] {
      override def pure[A](x: A): Test[F, A] = Test.Success(x)

      override def flatMap[A, B](fa: Test[F, A])(
          f: A => Test[F, B]): Test[F, B] = fa match {
        case Defer(test)              => Defer(test.map(flatMap(_)(f)))
        case error: Test.Error[F]     => error
        case failure: Test.Failure[F] => failure
        case Test.Group(tests)        => Test.Group(tests.map(flatMap(_)(f)))
        case Test.Label(description, test) =>
          Test.Label(description, flatMap(test)(f))
        case Test.Message(description, test) =>
          Test.Message(description, flatMap(test)(f))
        case Test.Skip(test)     => Test.Skip(flatMap(test)(f))
        case Test.Success(value) => f(value)
      }

      override def tailRecM[A, B](a: A)(
          f: A => Test[F, Either[A, B]]): Test[F, B] = {
        def go(test: Test[F, Either[A, B]]): Test[F, B] = test match {
          case Defer(test)              => Defer(test.map(go))
          case error: Test.Error[F]     => error
          case failure: Test.Failure[F] => failure
          case Test.Group(tests)        => Test.Group(tests.map(go))
          case Test.Label(description, test) =>
            Test.Label(description, go(test))
          case Test.Message(description, test) =>
            Test.Message(description, go(test))
          case Test.Skip(test)        => Test.Skip(go(test))
          case Test.Success(Right(b)) => Test.Success(b)
          case Test.Success(Left(a))  => go(f(a))
        }

        go(f(a))
      }
    }
}
