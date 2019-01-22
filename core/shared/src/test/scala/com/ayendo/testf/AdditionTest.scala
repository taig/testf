package com.ayendo.testf

import cats.effect.IO
import cats.implicits._
import cats.{Applicative, Eval, Id}

object AdditionTest extends TestF {
  def onePlusOne[F[_]](implicit F: Applicative[F]): Test[F] =
    condition[F](F.pure(1 + 1 === 2))

  def zeroPlusZero[F[_]](implicit F: Applicative[F]): Test[F] =
    equal[F, Int](F.pure(0 + 0), F.pure(0))

  val throwStuff: Test[IO] = condition[IO](IO.raiseError(new Exception))

  override val suite: List[IO[Summary]] =
    List(
      label("onePlusOneId", onePlusOne[Id]),
      label("zeroPlusZeroId", zeroPlusZero[Id]),
      label("zeroPlusZeroEval", onePlusOne[Eval]),
      label("zeroPlusZeroEval", zeroPlusZero[Eval]),
      label("zeroPlusZeroIO", onePlusOne[IO]),
      label("zeroPlusZeroIO", zeroPlusZero[IO])
    )
}
