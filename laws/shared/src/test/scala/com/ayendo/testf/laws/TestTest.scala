package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._
import com.ayendo.testf.dsl._

object TestTest extends TestF {
  import Generators._

  val eqLaws: Test[Pure] = Test.verify("EqLaws", EqTests[Test[Pure]].eqv)

  val semigroupLaws: Test[Pure] =
    Test.verify("SemigroupLaws", SemigroupTests[Test[Pure]].semigroup)

  override val suite: IO[Test[Pure]] =
    test("TestTest")(eqLaws, semigroupLaws).compile
}
