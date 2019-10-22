package io.taig.testf

import cats.effect.IO
import hedgehog._
import io.taig.testf.dsl._

object HedgehogTest extends TestApp {
  val reverse: Assertion[Pure] =
    test("reverse") {
      HedgehogAssertions.check(
        Gen.alpha
          .list(Range.linear(0, 100))
          .forAll
          .map(xs => xs.reverse.reverse ==== xs)
      )
    }

  override val suite: IO[Assertion[Pure]] =
    test("HedgehogTest")(reverse).interpret[IO]
}
