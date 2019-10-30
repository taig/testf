package io.taig.testf

import io.taig.testf.dsl._
import cats.implicits._
import cats.effect.IO

object AdditionTest extends IOTestApp {
  val onePlusOne: Assertion[Pure] =
    test("1 + 1 == 2")(isEqual(2)(1 + 1))

  val zeroPlusZero: Assertion[Pure] =
    test("0 + 0 == 0")(isEqual(0)(0 + 0))

  val negativePlusPositive: Assertion[Pure] =
    test("negativePlusPositive")(
      test("-1 + 1 == 0")(isEqual(0)(-1 + 1)),
      test("-1 + 0 == -1")(isEqual(-1)(-1 + 0))
    )

  val optionalAddition: Assertion[Pure] =
    test("optionalAddition")(isNotEmpty(3.some |+| none))

  val listSum: Assertion[Pure] = test("listSum")(isEmpty(List.empty[Int].sum))

  override val suite: IO[Assertion[Pure]] =
    test("AdditionTest")(
      onePlusOne,
      zeroPlusZero,
      negativePlusPositive,
      optionalAddition,
      listSum
    ).interpret[IO]

}
