package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Assert[Id] = Test[Id, Int]("onePlusOne", 1 + 1).equal(2)

  val zeroPlusZero: Assert[Id] = Test[Id, Int]("zeroPlusZero", 0 + 0).equal(0)

  override val suite: Assert[IO] = onePlusOne |+| zeroPlusZero liftIO
}
