package com.ayendo.testf

import cats.effect.IO
import cats.implicits._
import cats.{Applicative, Eval, Id}

object AdditionTest extends TestF {
  def onePlusOne[F[_]: Lift](flavor: String)(implicit F: Applicative[F]): Test =
    condition[F](s"onePlusOne$flavor", F.pure(1 + 1 === 2))

  def zeroPlusZero[F[_]: Lift](flavor: String)(
      implicit F: Applicative[F]): Test =
    equal[F, Int](s"zeroPlusZero$flavor", F.pure(0 + 0), F.pure(0))

  override val suite: List[Test] =
    List(
      onePlusOne[Id]("Id"),
      zeroPlusZero[Id]("Id"),
      onePlusOne[Eval]("Eval"),
      zeroPlusZero[Eval]("Eval"),
      onePlusOne[IO]("IO"),
      zeroPlusZero[IO]("IO")
    )
}
