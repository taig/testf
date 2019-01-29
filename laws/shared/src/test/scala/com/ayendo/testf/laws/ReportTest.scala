package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._

object ReportTest extends TestF {
  import Generators._

  override val suite: Test[IO, Unit] =
    (verify("EqLaws", EqTests[Report].eqv) |+|
      verify("SemigroupLaws", SemigroupTests[Report].semigroup)).liftIO
}
