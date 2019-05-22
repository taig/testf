package com.ayendo.testf

import cats.effect.{Effect, IO}
import cats.effect.implicits._
import cats.implicits._
import cats.{Eval, Id}
import simulacrum.typeclass

@typeclass
trait Compiler[F[_]] {
  def compile(test: Test[F]): IO[Test[Pure]]
}

object Compiler {
  implicit val id: Compiler[Id] = new Compiler[Id] {
    override def compile(test: Test[Id]): IO[Test[Pure]] = test match {
      case Test.Success            => IO.pure(Test.Success)
      case effect: Test.Effect[Id] => compile(effect.test)
      case group: Test.Group[Id] =>
        group.tests.traverse(compile).map(Test.group)
      case label: Test.Label[Id] =>
        compile(label.test).map(Test.Label(label.description, _))
      case message: Test.Message[Id] =>
        compile(message.test).map(Test.Message(message.description, _))
      case error: Test.Error     => IO.pure(error)
      case failure: Test.Failure => IO.pure(failure)
    }
  }

  implicit def effect[F[_]: Effect]: Compiler[F] = new Compiler[F] {
    override def compile(test: Test[F]): IO[Test[Pure]] =
      test match {
        case Test.Success => IO.pure(Test.Success)
        case effect: Test.Effect[F] =>
          effect.test.toIO.flatMap(compile).handleError(Test.failure)
        case group: Test.Group[F] =>
          group.tests.traverse(compile).map(Test.group)
        case label: Test.Label[F] =>
          compile(label.test).map(Test.Label(label.description, _))
        case message: Test.Message[F] =>
          compile(message.test).map(Test.Message(message.description, _))
        case error: Test.Error     => IO.pure(error)
        case failure: Test.Failure => IO.pure(failure)
      }
  }

  implicit val eval: Compiler[Eval] = new Compiler[Eval] {
    override def compile(test: Test[Eval]): IO[Test[Pure]] =
      test match {
        case Test.Success              => IO.pure(Test.Success)
        case effect: Test.Effect[Eval] => IO.eval(effect.test).flatMap(compile)
        case group: Test.Group[Eval] =>
          group.tests.traverse(compile).map(Test.group)
        case label: Test.Label[Eval] =>
          compile(label.test).map(Test.Label(label.description, _))
        case message: Test.Message[Eval] =>
          compile(message.test).map(Test.Label(message.description, _))
        case error: Test.Error     => IO.pure(error)
        case failure: Test.Failure => IO.pure(failure)
      }
  }

  implicit val pure: Compiler[Pure] = new Compiler[Pure] {
    override def compile(test: Test[Pure]): IO[Test[Pure]] = IO.pure(test)
  }
}
