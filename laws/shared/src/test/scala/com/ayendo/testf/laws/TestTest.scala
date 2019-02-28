package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._

object TestTest extends TestF {
  import Generators._

  override val suite: List[IO[Test]] =
    List(
      IO.pure(Test.verify("EqLaws", EqTests[Test].eqv)),
      IO.pure(Test.verify("SemigroupLaws", SemigroupTests[Test].semigroup)),
    )
}
