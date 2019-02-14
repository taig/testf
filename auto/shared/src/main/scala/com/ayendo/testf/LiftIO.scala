package com.ayendo.testf

import cats.{Eval, Id}
import cats.effect.IO
import simulacrum.typeclass

@typeclass
trait LiftIO[F[_]] {
  def lift[A](test: Test[F, A]): Test[IO, A]
}

object LiftIO {
  implicit val eval: LiftIO[Eval] = new LiftIO[Eval] {
    override def lift[A](test: Test[Eval, A]): Test[IO, A] =
      test.mapK(Test.liftEval)
  }

  implicit val id: LiftIO[Id] = new LiftIO[Id] {
    override def lift[A](test: Test[Id, A]): Test[IO, A] =
      test.mapK(Test.liftId)
  }

  implicit val io: LiftIO[IO] = new LiftIO[IO] {
    override def lift[A](test: Test[IO, A]): Test[IO, A] = test
  }
}
