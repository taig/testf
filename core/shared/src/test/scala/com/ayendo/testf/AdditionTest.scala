package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test[Id, Unit] = Test.equal("onePlusOne", 1 + 1, 2)

  val zeroPlusZero: Test[Id, Unit] = Test.equal("zeroPlusZero", 0 + 0, 0)

  override val suite: List[Test[IO, Unit]] =
    List(onePlusOne, zeroPlusZero).map(_.mapK(Test.liftId))
}
