package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.EqTests
import com.ayendo.testf._

object AssertionTest extends TestF {
  import Generators._

  override val suite: Test[IO, Assertion] =
    verify("EqLaws", EqTests[Assertion].eqv).liftIO
}
