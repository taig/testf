package com.ayendo.testf

import cats.effect.IO
import cats._
import cats.implicits._

sealed abstract class Test[+F[_]] extends Product with Serializable {
  final def root: List[Test[F]] = this match {
    case Test.Group(tests) => tests
    case test              => List(test)
  }
}

object Test extends Assertion {
  final case class Effect[F[_]](test: F[Test[F]]) extends Test[F]

  final case class Error(message: String) extends Test[Pure]

  final case class Failure(throwable: Throwable) extends Test[Pure]

  final case class Group[F[_]](tests: List[Test[F]]) extends Test[F]

  final case class Label[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case class Message[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case object Success extends Test[Pure]

  def assert(predicate: Boolean, message: => String): Test[Pure] =
    if (predicate) success else error(message)

  def effect[F[_]](test: F[Test[F]]): Test[F] = Test.Effect(test)

  def fallback[F[_]](description: String, test: Test[F]): Test[F] =
    test match {
      case label: Label[F] => label
      case test            => label(description, test)
    }

  def group[F[_]](tests: List[Test[F]]): Test[F] =
    Test.Group(tests)

  def label[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Label(description, test)

  def message[F[_]](description: String, test: Test[F]): Test[F] =
    Test.Message(description, test)

  def not[F[_]](test: Test[F]): Test[Pure] = ???

  def of[F[_]](tests: Test[F]*): Test[F] = group(tests.toList)

  val success: Test[Pure] = Test.Success

  def success(description: String): Test[Pure] =
    label(description, success)

  def error(message: String): Test[Pure] = Error(message)

  def failure(throwable: Throwable): Test[Pure] =
    Test.Failure(throwable)

  implicit def semigroup[F[_]]: Semigroup[Test[F]] = new Semigroup[Test[F]] {
    override def combine(x: Test[F], y: Test[F]): Test[F] =
      (x, y) match {
        case (x: Group[F], y: Group[F]) => Group(x.tests ++ y.tests)
        case (x: Group[F], test)        => Group(x.tests :+ test)
        case (test, y: Group[F])        => Group(test +: y.tests)
        case (x, y)                     => of(x, y)
      }
  }

  implicit val eq: Eq[Test[Pure]] = new Eq[Test[Pure]] {
    override def eqv(x: Test[Pure], y: Test[Pure]): Boolean =
      (x, y) match {
        case (Effect(_), Effect(_))             => false
        case (Error(x), Error(y))               => x === y
        case (Failure(x), Failure(y))           => x.getClass == y.getClass
        case (Group(x), Group(y))               => x === y
        case (Label(xd, xt), Label(yd, yt))     => xd === yd && xt === yt
        case (Message(xd, xt), Message(yd, yt)) => xd === yd && xt === yt
        case (Success, Success)                 => true
        case _                                  => false
      }
  }

  implicit final class TestOps[F[_]](val test: Test[F]) extends AnyVal {
    def compile(implicit compiler: Compiler[F]): IO[Test[Pure]] =
      compiler.compile(test)

    def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Test[G] = test match {
      case effect: Effect[F] => Effect(f(effect.test.map(_.mapK(f))))
      case error: Error      => error
      case failure: Failure  => failure
      case group: Group[F]   => Group(group.tests.map(_.mapK(f)))
      case label: Label[F]   => Label(label.description, label.test.mapK(f))
      case message: Message[F] =>
        Message(message.description, message.test.mapK(f))
      case Success => Success
    }

    def label(description: String): Test[F] = Test.label(description, test)

    def ~(description: String): Test[F] = label(description)
  }

  implicit final class TestPureOps(val test: Test[Pure]) extends AnyVal {
    def children: List[Test[Pure]] = test match {
      case effect: Effect[Pure] => effect.test.children
      case error: Error         => List(error)
      case failure: Failure     => List(failure)
      case Group(tests)         => tests.flatMap(_.children)
      case Label(_, test)       => test.children
      case Message(_, test)     => test.children
      case Success              => List(Success)
    }

    def success: Boolean = test match {
      case effect: Effect[Pure]  => effect.test.success
      case group: Group[Pure]    => group.tests.forall(_.success)
      case Label(_, test)        => test.success
      case Message(_, test)      => test.success
      case _: Error | _: Failure => false
      case Success               => true
    }

    def throwable: Option[Throwable] = test match {
      case effect: Effect[Pure] => effect.test.throwable
      case Failure(throwable)   => Some(throwable)
      case group: Group[Pure] =>
        group.tests.map(_.throwable).collectFirst {
          case Some(throwable) => throwable
        }
      case Label(_, test)     => test.throwable
      case Message(_, test)   => test.throwable
      case _: Error | Success => None
    }
  }
}
