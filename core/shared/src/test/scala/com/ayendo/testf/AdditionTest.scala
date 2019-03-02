package com.ayendo.testf

import cats.effect.IO
import com.ayendo.testf.implicits._
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test = "onePlusOne" @@ Test.equal(1 + 1, 2)

  val zeroPlusZero: Test = "zeroPlusZero" @@ Test.equal(0 + 0, 0)

  override val suite: List[IO[Test]] =
    List(onePlusOne, zeroPlusZero).map(IO.pure)
}
