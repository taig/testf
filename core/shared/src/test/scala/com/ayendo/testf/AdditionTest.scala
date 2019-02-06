package com.ayendo.testf

import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test[Unit] = Test.equal("onePlusOne", 1 + 1, 2)

  val zeroPlusZero: Test[Unit] = Test.equal("zeroPlusZero", 0 + 0, 0)

  override val suite: IO[Test[Unit]] = IO.pure(onePlusOne |+| zeroPlusZero)
}
