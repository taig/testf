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
      label("onePlusOneId", onePlusOne[Id]).interpret,
      label("zeroPlusZeroId", zeroPlusZero[Id]).interpret,
      label("zeroPlusZeroEval", onePlusOne[Eval]).interpret,
      label("zeroPlusZeroEval", zeroPlusZero[Eval]).interpret,
      label("zeroPlusZeroIO", onePlusOne[IO]).interpret,
      label("zeroPlusZeroIO", zeroPlusZero[IO]).interpret,
    )
}
