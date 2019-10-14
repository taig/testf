package io.taig.testf.hedgehog

import cats.effect.IO
import hedgehog._
import io.taig.testf._
import io.taig.testf.dsl._

object HedgehogTest extends TestF {
  def reverse: Assertion =
    test("reverse") {
      check(
        Gen.alpha
          .list(Range.linear(0, 100))
          .forAll
          .map(xs => xs.reverse.reverse ==== xs)
      )
    }

  override val suite: IO[Assertion] =
    test("HedgehogTest")(reverse).compile
}
