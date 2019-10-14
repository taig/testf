package io.taig.testf

import io.taig.testf.dsl._
import cats.implicits._
import cats.effect.IO

object AdditionTest extends TestApp {
  val onePlusOne: Assertion[Pure] =
    test("1 + 1 == 2")(equal(2)(1 + 1))

  val zeroPlusZero: Assertion[Pure] =
    test("0 + 0 == 0")(equal(0)(0 + 0))

  val negativePlusPositive: Assertion[Pure] =
    test("negativePlusPositive")(
      test("-1 + 1 == 0")(equal(0)(-1 + 1)),
      test("-1 + 0 == -1")(equal(-1)(-1 + 0))
    )

  val optionalAddition: Assertion[Pure] =
    test("optionalAddition")(notEmpty(3.some |+| none))

  val listSum: Assertion[Pure] = test("listSum")(isEmpty(List.empty[Int].sum))

  override val suite: IO[Assertion[Pure]] =
    test("AdditionTest")(
      onePlusOne,
      zeroPlusZero,
      negativePlusPositive,
      optionalAddition,
      listSum
    ).compile

}
