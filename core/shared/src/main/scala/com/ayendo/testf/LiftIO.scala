package com.ayendo.testf

import cats.{Eval, Id}
import cats.arrow.FunctionK
import cats.effect.IO
import simulacrum.typeclass

@typeclass
trait LiftIO[F[_]] {
  def lift: FunctionK[F, IO]
}

object LiftIO {
  implicit val eval: LiftIO[Eval] = new LiftIO[Eval] {
    override val lift: FunctionK[Eval, IO] = new FunctionK[Eval, IO] {
      override def apply[A](fa: Eval[A]): IO[A] = IO.eval(fa)
    }
  }

  implicit val id: LiftIO[Id] = new LiftIO[Id] {
    override val lift: FunctionK[Id, IO] = new FunctionK[Id, IO] {
      override def apply[A](fa: Id[A]): IO[A] = IO.pure(fa)
    }
  }

  implicit val io: LiftIO[IO] = new LiftIO[IO] {
    override val lift: FunctionK[IO, IO] = FunctionK.id
  }
}
