package com.ayendo.testf.laws

import cats.Id
import cats.effect.IO
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import com.ayendo.testf._
import com.ayendo.testf.implicits._

object TestTest extends TestF {
  import Generators._

  val eq: Test[Id] = "EqLaws" @@ Test.verify(EqTests[Test[Id]].eqv)

  val semigroup: Test[Id] = "SemigroupLaws" @@ Test.verify(
    SemigroupTests[Test[Id]].semigroup)

  override val suite: IO[Test[IO]] =
    IO.pure(Test.group(List(eq, semigroup).map(_.mapK(Test.liftId)): _*))
}
