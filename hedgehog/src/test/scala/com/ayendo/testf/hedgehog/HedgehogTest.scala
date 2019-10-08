package com.ayendo.testf.hedgehog

import cats.effect.IO
import hedgehog._
import com.ayendo.testf._

object HedgehogTest extends TestF {
  def reverse: Test[Pure] =
    Test.label(
      "reverse",
      HedgehogAssertion.check(
        Gen.alpha
          .list(Range.linear(0, 100))
          .forAll
          .map(xs => xs.reverse.reverse ==== xs)
      )
    )

  override val suite: IO[Test[Pure]] =
    Test.label("HedgehogTest", reverse).compile
}
