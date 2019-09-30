package com.ayendo.testf

import cats.effect.IO
import cats._
import cats.implicits._

sealed abstract class Test[+F[_]] extends Product with Serializable {
  def covary[G[α] >: F[α]]: Test[G] = this

  final def root: List[Test[F]] = this match {
    case Test.Group(tests) => tests
    case test              => List(test)
  }

  final def compile[G[α] >: F[α]](
      implicit compiler: Compiler[G]
  ): IO[Test[Pure]] =
    compiler.compile(this)

  final def mapK[G[α] >: F[α], H[_]](
      f: G ~> H
  )(implicit G: Functor[G]): Test[H] = covary[G] match {
    case test: Test.Effect[G] =>
      Test.Effect(f(test.test.map(_.mapK(f))))
    case error: Test.Error     => error
    case failure: Test.Failure => failure
    case group: Test.Group[G]  => Test.Group(group.tests.map(_.mapK(f)))
    case label: Test.Label[G] =>
      Test.Label(label.description, label.test.mapK(f))
    case message: Test.Message[G] =>
      Test.Message(message.description, message.test.mapK(f))
    case Test.Success => Test.Success
  }

  final def fold[G[α] >: F[α], A](
      effect: G[Test[G]] => A,
      error: String => A,
      failure: Throwable => A,
      group: List[Test[G]] => A,
      label: (String, Test[G]) => A,
      message: (String, Test[G]) => A,
      success: => A
  ): A = covary[G] match {
    case test: Test.Effect[G]    => effect(test.test)
    case Test.Error(message)     => error(message)
    case Test.Failure(throwable) => failure(throwable)
    case test: Test.Group[G]     => group(test.tests)
    case test: Test.Label[G]     => label(test.description, test.test)
    case test: Test.Message[G]   => message(test.description, test.test)
    case Test.Success            => success
  }

  final def ~(description: String): Test[F] =
    Test.label(description, this)

  final def &[G[α] >: F[α]](test: Test[G]): Test[G] =
    (this, test) match {
      case (x: Test.Group[F], y: Test.Group[G]) =>
        Test.Group(x.tests ++ y.tests)
      case (x: Test.Group[F], test) => Test.Group(x.tests :+ test)
      case (test, y: Test.Group[G]) => Test.Group(test +: y.tests)
      case (x, y)                   => Test.of(x, y)
    }
}

object Test extends Assertion {
  private final case class Effect[F[_]](test: F[Test[F]]) extends Test[F]

  private final case class Error(message: String) extends Test[Pure]

  private final case class Failure(throwable: Throwable) extends Test[Pure]

  private final case class Group[F[_]](tests: List[Test[F]]) extends Test[F]

  private final case class Label[F[_]](description: String, test: Test[F])
      extends Test[F]

  private final case class Message[F[_]](description: String, test: Test[F])
      extends Test[F]

  private final case object Success extends Test[Pure]

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

  def of[F[_]](tests: Test[F]*): Test[F] = group(tests.toList)

  val success: Test[Pure] = Test.Success

  def success(description: String): Test[Pure] =
    label(description, success)

  def error(message: String): Test[Pure] = Error(message)

  def failure(throwable: Throwable): Test[Pure] =
    Test.Failure(throwable)

  implicit def semigroup[F[_]]: Semigroup[Test[F]] = _ & _

  implicit val eq: Eq[Test[Pure]] = new Eq[Test[Pure]] {
    override def eqv(x: Test[Pure], y: Test[Pure]): Boolean =
      (x, y) match {
        case (Effect(_), Effect(_))             => false
        case (Error(x), Error(y))               => x === y
        case (Failure(x), Failure(y))           => x == y
        case (Group(x), Group(y))               => x === y
        case (Label(xd, xt), Label(yd, yt))     => xd === yd && xt === yt
        case (Message(xd, xt), Message(yd, yt)) => xd === yd && xt === yt
        case (Success, Success)                 => true
        case _                                  => false
      }
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
