package com.ayendo.testf

import cats.arrow.FunctionK
import cats.{Eval, Id}
import cats.effect.IO
import cats.implicits._
import simulacrum.typeclass

@typeclass
trait Interpreter[F[_]] {
  def interpret(result: Test.Assert[F]): IO[Summary]
}

object Interpreter {
  implicit val id: Interpreter[Id] = {
    case Test.Assert(description, Result.Success) =>
      IO.pure(Summary.Success(description))
    case Test.Assert(description, Result.Error(message)) =>
      IO.pure(Summary.Error(description, message))
  }

  implicit val io: Interpreter[IO] = {
    case Test.Assert(description, result) =>
      result
        .map {
          case Result.Success        => Summary.Success(description)
          case Result.Error(message) => Summary.Error(description, message)
        }
        .handleError { throwable =>
          Summary.Failure(description, throwable)
        }
  }

  implicit val eval: Interpreter[Eval] =
    _.mapK(FunctionK.lift(IO.eval)).interpret
}
