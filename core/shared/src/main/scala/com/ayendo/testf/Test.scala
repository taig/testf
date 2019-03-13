package com.ayendo.testf

import cats._
import cats.arrow.FunctionK
import cats.effect.IO
import cats.implicits._

sealed trait Test[+F[_]] extends Product with Serializable

object Test extends TestBuilders {
  final case class Defer[F[_]](test: F[Test[F]]) extends Test[F]

  final case class Error(message: String) extends Test[Nothing]

  final case class Failure(throwable: Throwable) extends Test[Nothing]

  final case class Group[F[_]](tests: List[Test[F]]) extends Test[F]

  final case class Label[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case class Message[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case object Success extends Test[Nothing]

  final case class Result(test: Test[Id], duration: Option[Long])

  def liftId: FunctionK[Id, IO] = new (Id ~> IO) {
    override def apply[A](fa: Id[A]): IO[A] = IO.pure(fa)
  }

  def liftEval: FunctionK[Eval, IO] = new (Eval ~> IO) {
    override def apply[A](fa: Eval[A]): IO[A] = IO.eval(fa)
  }

  implicit val eq: Eq[Test[Id]] =
    new Eq[Test[Id]] {
      override def eqv(x: Test[Id], y: Test[Id]): Boolean = {
        PartialFunction.cond((x, y)) {
          case (Defer(x), Defer(y))             => eqv(x, y)
          case (Error(m1), Error(m2))           => m1 === m2
          case (Failure(t1), Failure(t2))       => t1 == t2
          case (Group(xs), Group(ys))           => xs === ys
          case (Label(dx, x), Label(dy, y))     => dx === dy && eqv(x, y)
          case (Message(dx, x), Message(dy, y)) => dx === dy && eqv(x, y)
          case (Success, Success)               => true
        }
      }
    }

  implicit def semigroup[F[_]]: Semigroup[Test[F]] = {
    case (x: Group[F], y: Group[F]) => Group(x.tests |+| y.tests)
    case (x: Group[F], y)           => Group(x.tests :+ y)
    case (x, y: Group[F])           => Group(x +: y.tests)
    case (x, y)                     => Group(List(x, y))
  }

  implicit val semigroupNothing: Semigroup[Test[Nothing]] = semigroup[Nothing]

  implicit def show[F[_]]: Show[Test[F]] =
    new Show[Test[F]] {
      override def show(test: Test[F]): String = test match {
        case test: Defer[F]     => s"Defer(${test.test})"
        case Error(message)     => s"Error($message)"
        case Failure(throwable) => s"Failure(${throwable.getMessage})"
        case test: Group[F]     => s"Group(${test.tests.map(show).mkString(", ")})"
        case test: Label[F]     => s"Label(${test.description}, ${show(test.test)})"
        case test: Message[F] =>
          s"Message(${test.description}, ${show(test.test)})"
        case Success => s"Success"
      }
    }

  implicit def testOps[F[_]: Monad](test: Test[F]): TestOps[F] =
    new TestOps[F](test)
}
