package com.ayendo.testf

import cats.Id
import cats.implicits._
import cats.effect.Sync
import simulacrum.typeclass

@typeclass
trait Measure[F[_]] {
  def measure[A](fa: F[A]): F[(A, Option[Long])]
}

object Measure {
  implicit val id: Measure[Id] = new Measure[Id] {
    override def measure[A](fa: Id[A]): (A, Option[Long]) = (fa, None)
  }

  implicit def sync[F[_]: Sync]: Measure[F] = new Measure[F] {
    override def measure[A](fa: F[A]): F[(A, Option[Long])] =
      for {
        start <- Sync[F].delay(System.currentTimeMillis())
        fa <- fa
        end <- Sync[F].delay(System.currentTimeMillis())
      } yield (fa, Some(end - start))
  }
}
