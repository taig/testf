package com.ayendo.testf

import cats.Id
import cats.effect.IO
import com.ayendo.testf.implicits._
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Test[Id] = "onePlusOne" @@ Test.equal(1 + 1, 2)

  val zeroPlusZero: Test[Id] = "zeroPlusZero" @@ Test.equal(0 + 0, 0)

  override val suite: IO[Test[IO]] =
    IO.pure(
      Test.group(
        onePlusOne.mapK(Test.liftId),
        zeroPlusZero.mapK(Test.liftId)
      )
    )
}
