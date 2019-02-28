package com.ayendo.testf

import cats.{Eval, Id}
import cats.effect.IO
import simulacrum.typeclass

@typeclass
trait LiftIO[F[_]] {
  def lift(test: F[Test]): IO[Test]
}

object LiftIO {
  implicit val eval: LiftIO[Eval] = IO.eval

  implicit val id: LiftIO[Id] = IO.pure

  implicit val io: LiftIO[IO] = identity
}
