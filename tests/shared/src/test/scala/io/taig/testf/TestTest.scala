package io.taig.testf

import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import io.taig.testf.dsl._
import io.taig.testf.Cogens.cogenTest
import org.scalacheck.Arbitrary

object TestTest extends TestApp {
  implicit val arbitrary: Arbitrary[Assertion[Pure]] = Arbitrary(
    Generators.test
  )

  val eqLaws: Assertion[Pure] =
    LawsAssertions.verify("EqLaws", EqTests[Assertion[Pure]].eqv)

  val semigroupLaws: Assertion[Pure] = LawsAssertions.verify(
    "SemigroupLaws",
    SemigroupTests[Assertion[Pure]].semigroup
  )

  override val suite: IO[Assertion[Pure]] =
    test("TestTest")(eqLaws, semigroupLaws).interpret[IO]
}
