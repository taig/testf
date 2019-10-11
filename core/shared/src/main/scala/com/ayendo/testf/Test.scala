package com.ayendo.testf

import cats.effect.IO
import cats._
import cats.implicits._

import scala.annotation.tailrec

sealed abstract class Test[+F[_]] extends Product with Serializable {
  def covary[G[α] >: F[α]]: Test[G] = this

  final def compile[G[α] >: F[α]](
      implicit compiler: Compiler[G]
  ): IO[Test[Pure]] =
    compiler.compile(this)

  final def mapK[G[α] >: F[α], H[_]](
      f: G ~> H
  )(implicit G: Functor[G]): Test[H] = covary[G] match {
    case test: Test.And[G]     => Test.And(test.tests.map(_.mapK(f)))
    case test: Test.Eval[G]    => Test.Eval(f(test.test.map(_.mapK(f))))
    case error: Test.Error     => error
    case failure: Test.Failure => failure
    case label: Test.Label[G] =>
      Test.Label(label.description, label.test.mapK(f))
    case message: Test.Message[G] =>
      Test.Message(message.description, message.test.mapK(f))
    case test: Test.Not[G]  => Test.Not(test.test.mapK(f))
    case test: Test.Or[G]   => Test.Or(test.tests.map(_.mapK(f)))
    case test: Test.Skip[G] => Test.Skip(test.test.mapK(f))
    case Test.Success       => Test.Success
  }

  final def and[G[α] >: F[α]](test: Test[G]): Test[G] = (this, test) match {
    case (Test.And(x), y: Test.And[G]) => Test.And[G](x ++ y.tests)
    case (Test.And(x), y)              => Test.And[G](x :+ y)
    case (x, y: Test.And[G])           => Test.And[G](x +: y.tests)
    case (x, y)                        => Test.And(List(x, y))
  }

  final def &[G[α] >: F[α]](test: Test[G]): Test[G] = this and test

  final def or[G[α] >: F[α]](test: Test[G]): Test[G] = (this, test) match {
    case (Test.Or(x), y: Test.Or[G]) => Test.Or(x ++ y.tests)
    case (Test.Or(x), y)             => Test.Or[G](x :+ y)
    case (x, y: Test.Or[G])          => Test.Or[G](x +: y.tests)
    case (x, y)                      => Test.Or(List(x, y))
  }

  final def |[G[α] >: F[α]](test: Test[G]): Test[G] = this or test
}

object Test extends Builder {
  final case class And[F[_]](tests: List[Test[F]]) extends Test[F]

  final case class Eval[F[_]](test: F[Test[F]]) extends Test[F]

  final case class Error(message: String) extends Test[Pure]

  final case class Failure(throwable: Throwable) extends Test[Pure]

  final case class Label[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case class Message[F[_]](description: String, test: Test[F])
      extends Test[F]

  final case class Not[F[_]](test: Test[F]) extends Test[F]

  final case class Or[F[_]](tests: List[Test[F]]) extends Test[F]

  final case class Skip[F[_]](test: Test[F]) extends Test[F]

  final case object Success extends Test[Pure]

  implicit def monoid[F[_]]: Monoid[Test[F]] = new Monoid[Test[F]] {
    override def empty: Test[F] = Test.empty

    override def combine(x: Test[F], y: Test[F]): Test[F] = x & y
  }

  implicit val eq: Eq[Test[Pure]] = new Eq[Test[Pure]] {
    override def eqv(x: Test[Pure], y: Test[Pure]): Boolean =
      (x, y) match {
        case (And(x), And(y))               => x === y
        case (And(x :: Nil), y)             => x === y
        case (x, And(y :: Nil))             => x === y
        case (x: Eval[Pure], y: Eval[Pure]) => eqv(x.test, y.test)
        case (Error(x), Error(y))           => x === y
        case (Failure(x), Failure(y)) =>
          x.getMessage === y.getMessage && x.getClass == y.getClass
        case (Label(xd, xt), Label(yd, yt))     => xd === yd && eqv(xt, yt)
        case (Message(xd, xt), Message(yd, yt)) => xd === yd && eqv(xt, yt)
        case (Not(x), Not(y))                   => eqv(x.test, y.test)
        case (Or(x), Or(y))                     => x === y
        case (Skip(x), Skip(y))                 => eqv(x, y)
        case (Success, Success)                 => true
        case _                                  => false
      }
  }

  implicit final class TestPureOps(val test: Test[Pure]) extends AnyVal {
    @tailrec
    def label: Option[String] = test match {
      case Test.And(_)                => none
      case test: Test.Eval[Pure]      => test.label
      case Test.Error(_)              => none
      case Test.Failure(_)            => none
      case Test.Label(description, _) => description.some
      case Test.Message(_, test)      => test.label
      case Test.Not(test)             => test.label
      case Test.Or(_)                 => none
      case Test.Skip(test)            => test.label
      case Test.Success               => none
    }

    def children: List[Test[Pure]] = test match {
      case And(tests)       => tests
      case eval: Eval[Pure] => eval.test.children
      case error: Error     => List(error)
      case failure: Failure => List(failure)
      case Label(_, test)   => test.children
      case Message(_, test) => test.children
      case Not(test)        => test.children
      case Or(tests)        => tests
      case Skip(_)          => List.empty
      case Success          => List(Success)
    }

    def success: Boolean = test match {
      case And(tests)              => tests.forall(_.success)
      case eval: Eval[Pure]        => eval.test.success
      case Label(_, test)          => test.success
      case Message(_, test)        => test.success
      case Not(test)               => !test.success
      case Or(tests)               => tests.exists(_.success)
      case _: Error | _: Failure   => false
      case _: Skip[Pure] | Success => true
    }

    def throwable: Option[Throwable] = test match {
      case And(tests)                         => tests.collectFirstSome(_.throwable)
      case eval: Eval[Pure]                   => eval.test.throwable
      case Failure(throwable)                 => Some(throwable)
      case Label(_, test)                     => test.throwable
      case Message(_, test)                   => test.throwable
      case Not(test)                          => test.throwable
      case Or(tests)                          => tests.collectFirstSome(_.throwable)
      case _: Error | _: Skip[Pure] | Success => None
    }
  }
}
