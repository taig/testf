package com.ayendo.testf.laws

import cats.effect.IO
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._
import com.ayendo.testf.implicits._

object TestTest extends TestF {
  import Generators._

  val eq: Test = "EqLaws" @@ Test.verify(EqTests[Test].eqv)

  val semigroup: Test = "SemigroupLaws" @@ Test.verify(
    SemigroupTests[Test].semigroup)

  override val suite: IO[List[IO[Test]]] =
    IO.pure(List(eq, semigroup).map(IO.pure))
}
