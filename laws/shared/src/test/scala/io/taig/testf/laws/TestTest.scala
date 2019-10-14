package io.taig.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import io.taig.testf._
import io.taig.testf.dsl._
import io.taig.testf.laws.Cogens.cogenTest
import org.scalacheck.Arbitrary

object TestTest extends TestF {
  implicit val arbitrary: Arbitrary[Assertion] = Arbitrary(Generators.test)

  val eqLaws: Assertion = verify("EqLaws", EqTests[Assertion].eqv)

  val semigroupLaws: Assertion =
    verify("SemigroupLaws", SemigroupTests[Assertion].semigroup)

  override val suite: IO[Assertion] =
    test("TestTest")(eqLaws, semigroupLaws).compile
}
