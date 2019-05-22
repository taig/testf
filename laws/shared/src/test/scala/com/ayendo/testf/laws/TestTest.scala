package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._

object TestTest extends TestF {
  import Generators._

  val eq: Test[Pure] = Test.verify("EqLaws", EqTests[Test[Pure]].eqv)

  val semigroup: Test[Pure] =
    Test.verify("SemigroupLaws", SemigroupTests[Test[Pure]].semigroup)

  override val suite: IO[Test[Pure]] =
    (Test.of(eq, semigroup) label "TestTest").compile
}
