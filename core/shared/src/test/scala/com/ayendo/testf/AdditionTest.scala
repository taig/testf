package com.ayendo.testf

import cats.effect.IO
import cats.implicits._
import cats.{Applicative, Eval, Id}

object AdditionTest extends TestF {
  def onePlusOne[F[_]: Lift](implicit F: Applicative[F]): Test =
    condition[F](F.pure(1 + 1 === 2))

  def zeroPlusZero[F[_]: Lift](implicit F: Applicative[F]): Test =
    equal[F, Int](F.pure(0 + 0), F.pure(0))

  override val suite: List[Test] =
    List(
      label("onePlusOneId", onePlusOne[Id]),
      label("zeroPlusZeroId", zeroPlusZero[Id]),
      label("zeroPlusZeroEval", onePlusOne[Eval]),
      label("zeroPlusZeroEval", zeroPlusZero[Eval]),
      label("zeroPlusZeroIO", onePlusOne[IO]),
      label("zeroPlusZeroIO", zeroPlusZero[IO])
    )
}
