package com.ayendo.testf

import cats.arrow.FunctionK
import cats.{Eval, Id}
import cats.effect.IO
import simulacrum.typeclass

@typeclass
trait LiftIO[F[_]] {
  def lift(test: Test[F]): Test[IO]
}

object LiftIO {
  implicit val eval: LiftIO[Eval] = _.mapK(Test.liftEval)

  implicit val id: LiftIO[Id] = _.mapK(Test.liftId)

  implicit val io: LiftIO[IO] = _.mapK(FunctionK.id[IO])
}
