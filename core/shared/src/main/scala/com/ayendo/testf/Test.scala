package com.ayendo.testf

import cats._
import cats.effect.IO
import cats.implicits._

sealed trait Test[F[_], A] extends Product with Serializable {
  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Test[G, A] = this match {
    case error: Test.Error[F, A]       => error.asInstanceOf[Test.Error[G, A]]
    case failure: Test.Failure[F, A]   => failure.asInstanceOf[Test.Failure[G, A]]
    case Test.Group(tests)             => Test.Group(tests.map(_.mapK(f)))
    case Test.Label(description, test) => Test.Label(description, test.mapK(f))
    case pure: Test.Pure[F, A]         => pure.asInstanceOf[Test[G, A]]
    case Test.Skip(test)               => Test.Skip(test.mapK(f))
    case success: Test.Success[F, A]   => success.asInstanceOf[Test.Success[G, A]]
    case Test.Suspend(test)            => Test.Suspend(f(test.map(_.mapK(f))))
  }

  def liftIO(implicit F: Functor[F], L: LiftIO[F]): Test[IO, A] = mapK(L.lift)
}

object Test {
  final case class Error[F[_], A](message: String) extends Test[F, A]

  final case class Failure[F[_], A](throwable: Throwable) extends Test[F, A]

  final case class Group[F[_], A](tests: List[Test[F, A]]) extends Test[F, A]

  final case class Label[F[_], A](description: String, test: Test[F, A])
      extends Test[F, A]

  final case class Pure[F[_], A](value: A) extends Test[F, A]

  final case class Skip[F[_], A](test: Test[F, A]) extends Test[F, A]

  final case class Success[F[_], A]() extends Test[F, A]

  final case class Suspend[F[_], A](test: F[Test[F, A]]) extends Test[F, A]

  def apply[F[_], A](description: String, value: A): Test[F, A] =
    pure(description, value)

  def effect[F[_]: Functor, A](description: String, value: F[A]): Test[F, A] =
    label(description, Suspend[F, A](value.map(Pure.apply)))

  def label[F[_], A](description: String, test: Test[F, A]): Test[F, A] =
    Label(description, test)

  def pure[F[_], A](description: String, value: A): Test[F, A] =
    label(description, Pure(value))

  def success[F[_], A](description: String): Test[F, A] =
    label(description, Success())

  def error[F[_], A](description: String, message: String): Test[F, A] =
    label(description, Error(message))

  def failure[F[_], A](description: String, throwable: Throwable): Test[F, A] =
    label(description, Failure(throwable))

  def skip[F[_], A](test: Test[F, A]): Test[F, A] = Skip(test)

  def susped[F[_]: Functor, A](test: F[Test[F, A]]): Test[F, A] = Suspend(test)

  implicit def monad[F[_]: Functor]: Monad[Test[F, ?]] = new Monad[Test[F, ?]] {
    override def pure[A](x: A): Test[F, A] = Pure(x)

    override def flatMap[A, B](fa: Test[F, A])(f: A => Test[F, B]): Test[F, B] =
      fa match {
        case error: Error[F, A]       => error.asInstanceOf[Error[F, B]]
        case failure: Failure[F, A]   => failure.asInstanceOf[Failure[F, B]]
        case Group(tests)             => Group(tests.map(flatMap(_)(f)))
        case Label(description, test) => Label(description, flatMap(test)(f))
        case Pure(value)              => f(value)
        case Skip(test)               => Skip(flatMap(test)(f))
        case success: Success[F, A]   => success.asInstanceOf[Success[F, B]]
        case Suspend(test)            => Suspend(test.map(flatMap(_)(f)))
      }

    override def tailRecM[A, B](a: A)(
        f: A => Test[F, Either[A, B]]): Test[F, B] = {
      def go(test: Test[F, Either[A, B]]): Test[F, B] = test match {
        case error: Error[F, _]       => error.asInstanceOf[Error[F, B]]
        case failure: Failure[F, _]   => failure.asInstanceOf[Failure[F, B]]
        case Group(tests)             => Group(tests.map(go))
        case Label(description, test) => Label(description, go(test))
        case Pure(Right(b))           => Pure(b)
        case Pure(Left(a))            => go(f(a))
        case Skip(test)               => Skip(go(test))
        case success: Success[F, _]   => success.asInstanceOf[Success[F, B]]
        case Suspend(test)            => Suspend(test.map(go))
      }

      go(f(a))
    }
  }

  implicit def monadId: Monad[Test[Id, ?]] = monad[Id]

  implicit def semigroup[F[_], A]: Semigroup[Test[F, A]] = {
    case (Group(xs), Group(ys)) => Group(xs |+| ys)
    case (Group(xs), y)         => Group(xs :+ y)
    case (x, Group(ys))         => Group(x +: ys)
    case (x, y)                 => Group(List(x, y))
  }

  implicit def eq[A: Eq]: Eq[Test[Id, A]] = new Eq[Test[Id, A]] {
    override def eqv(x: Test[Id, A], y: Test[Id, A]): Boolean = {
      PartialFunction.cond((x, y)) {
        case (Error(m1), Error(m2))       => m1 === m2
        case (Failure(t1), Failure(t2))   => t1 == t2
        case (Group(xs), Group(ys))       => xs === ys
        case (Label(dx, x), Label(dy, y)) => dx === dy && eqv(x, y)
        case (Pure(x), Pure(y))           => x === y
        case (Skip(x), Skip(y))           => eqv(x, y)
        case (Success(), Success())       => true
        case (Suspend(x), Suspend(y))     => eqv(x, y)
      }
    }
  }

  implicit def show[F[_], A: Show]: Show[Test[F, A]] = new Show[Test[F, A]] {
    override def show(test: Test[F, A]): String = test match {
      case Error(message)           => s"Error($message)"
      case Failure(throwable)       => s"Failure(${throwable.getMessage})"
      case Group(tests)             => s"Group(${tests.map(show).mkString(", ")})"
      case Label(description, test) => s"Label($description, ${show(test)})"
      case Pure(value)              => show"Pure($value)"
      case Skip(test)               => s"Skip(${show(test)})"
      case Success()                => s"Success()"
      case Suspend(test)            => s"Defer($test)"
    }
  }

  implicit def testOps[F[_], A](test: Test[F, A]): TestOps[F, A] =
    new TestOps(test)

  implicit def testOpsBoolean[F[_]](test: Test[F, Boolean]): TestOpsBoolean[F] =
    new TestOpsBoolean(test)

  implicit def testOpsMonoid[F[_], A](test: Test[F, A]): TestOpsMonoid[F, A] =
    new TestOpsMonoid[F, A](test)

  implicit class TestOpsResult[F[_]](val test: Test[F, Assertion])
      extends AnyVal {
    def run(implicit F: Monad[F]): F[Summary] = Test.run(None, test)
  }

  private def run[F[_]](description: Option[String], test: Test[F, Assertion])(
      implicit F: Monad[F]): F[Summary] =
    (description, test) match {
      case (description, Suspend(test)) => test.flatMap(run(description, _))
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
    }
}
