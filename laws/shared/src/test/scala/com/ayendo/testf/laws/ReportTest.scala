package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.EqTests
import com.ayendo.testf._

object ReportTest extends TestF {
  import Generators._

  override val suite: Assert[IO] =
    verify("EqLaws", EqTests[Report].eqv).liftIO
}
