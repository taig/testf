package com.ayendo.testf

import cats._
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.Test.{Assert, Group, Label}

sealed trait Test[F[_]] extends Product with Serializable {
  def mapK[G[_]](f: F ~> G): Test[G] = this match {
    case Test.Assert(description, result) => Test.Assert(description, f(result))
    case Test.Group(left, right)          => Test.Group(left.mapK(f), right.mapK(f))
    case Test.Label(description, test)    => Test.Label(description, test.mapK(f))
  }

  def run(implicit interpreter: AssertRunner[F]): IO[Summary] =
    this match {
      case assert: Assert[F] => interpreter.run(assert)
      case Group(left, right) =>
        (left.run, right.run).mapN { (left, right) =>
          Summary.Group(left, right, description = None)
        }
      case Label(description, Assert(_, result)) =>
        Assert(description, result).run
      case Label(description, Group(left, right)) =>
        (left.run, right.run).mapN { (left, right) =>
          Summary.Group(left, right, description = Some(description))
        }
      case Label(description, Label(_, test)) =>
        Label(description, test).run
    }
}

object Test {
  case class Assert[F[_]](description: String, result: F[Result])
      extends Test[F]

  case class Group[F[_]](left: Test[F], right: Test[F]) extends Test[F]

  case class Label[F[_]](description: String, test: Test[F]) extends Test[F]

  implicit def semigroup[F[_]]: Semigroup[Test[F]] = Group.apply

  implicit def run[F[_]: AssertRunner](test: Test[F]): IO[Summary] = test.run
}
