package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test[Id, Assertion] = value("onePlusOne", 1 + 1).equal(2)

  val zeroPlusZero: Test[Id, Assertion] = value("zeroPlusZero", 0 + 0).equal(0)

  override val suite: Test[IO, Assertion] = (onePlusOne |+| zeroPlusZero).liftIO
}
