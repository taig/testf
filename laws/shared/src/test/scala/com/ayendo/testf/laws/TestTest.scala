package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object TestTest extends TestF {
  import Generators._

  override val suite: IO[Test[Unit]] =
    IO.pure(
      Test.verify("EqLaws", EqTests[Test[Int]].eqv) |+|
        Test.verify("SemigroupLaws", SemigroupTests[Test[Int]].semigroup) |+|
        Test.verify("MonadLaws",
                    MonadTests[Test].stackUnsafeMonad[Int, Int, String])
    )
}
