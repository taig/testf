package com.ayendo.testf

import cats.{Eval, Id}
import cats.effect.IO
import simulacrum.typeclass

@typeclass
trait Lift[F[_]] {
  def toIO[A](value: F[A]): IO[A]
}

object Lift {
  implicit val id: Lift[Id] = new Lift[Id] {
    override def toIO[A](value: Id[A]): IO[A] = IO.pure(value)
  }

  implicit val eval: Lift[Eval] = new Lift[Eval] {
    override def toIO[A](value: Eval[A]): IO[A] = IO.eval(value)
  }

  implicit val io: Lift[IO] = new Lift[IO] {
    override def toIO[A](value: IO[A]): IO[A] = value
  }
}
