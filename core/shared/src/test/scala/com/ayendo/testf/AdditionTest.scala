package com.ayendo.testf

import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test = Test.equal("onePlusOne", 1 + 1, 2)

  val zeroPlusZero: Test = Test.equal("zeroPlusZero", 0 + 0, 0)

  override val suite: List[IO[Test]] =
    List(onePlusOne, zeroPlusZero).map(IO.pure)
}
