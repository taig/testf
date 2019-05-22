package com.ayendo.testf

import cats.implicits._
import cats.effect.IO

object AdditionTest extends TestF {
  val onePlusOne =
    Test.label("onePlusOne", Test.assert(1 + 1 == 2, "1 + 1 == 2"))

  val zeroPlusZero =
    Test.label("zeroPlusZero", Test.assert(0 + 0 == 0, "0 + 0 == 0"))

  val negativePlusPositive =
    Test.of(
      Test.label("plusOne", Test.assert(-1 + 1 == 0, "-1 + 1 == 0")),
      Test.label("plusZero", Test.assert(-1 + 0 == -1, "-1 + 0 == -1"))
    )

  val optionalAddition =
    Test.label("optionalAddition", Test.notEmpty(3.some |+| none))

  val listSum = Test.label("listSum", Test.isEmpty(List.empty[Int].sum))

  override val suite: IO[Test[Pure]] =
    Compiler[Pure].compile(
      Test.label(
        "AdditionTest",
        Test.of(
          onePlusOne,
          zeroPlusZero,
          negativePlusPositive,
          optionalAddition,
          listSum
        )
      )
    )
}
