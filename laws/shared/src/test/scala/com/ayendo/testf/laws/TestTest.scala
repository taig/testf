package com.ayendo.testf.laws

import cats.Id
import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object TestTest extends TestF {
  import Generators._

  override val suite: Assert[IO] =
    (
      verify("EqLaws", EqTests[Test[Id, Int]].eqv) |+|
        verify("SemigroupLaws", SemigroupTests[Test[Id, Int]].semigroup) |+|
        verify("MonadLaws",
               MonadTests[Test[Id, ?]].stackUnsafeMonad[Int, Int, String])
    ).liftIO
}
