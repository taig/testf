package io.taig.testf

import io.taig.testf.dsl._
import cats.implicits._
import cats.effect.IO

object AdditionTest extends TestF {
  val onePlusOne: Assertion =
    test("1 + 1 == 2")(equal(2)(1 + 1))

  val zeroPlusZero: Assertion =
    test("0 + 0 == 0")(equal(0)(0 + 0))

  val negativePlusPositive: Assertion =
    test("negativePlusPositive")(
      test("-1 + 1 == 0")(equal(0)(-1 + 1)),
      test("-1 + 0 == -1")(equal(-1)(-1 + 0))
    )

  val optionalAddition: Assertion =
    test("optionalAddition")(notEmpty(3.some |+| none))

  val listSum: Assertion = test("listSum")(isEmpty(List.empty[Int].sum))

  override val suite: IO[Assertion] =
    test("AdditionTest")(
      onePlusOne,
      zeroPlusZero,
      negativePlusPositive,
      optionalAddition,
      listSum
    ).compile

}
