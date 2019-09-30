package com.ayendo.testf.laws

import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit val arbitraryTest: Arbitrary[Test[Pure]] = {
    val error =
      for {
        description <- description
        message <- summon[String]
      } yield Test.error(message) ~ description

    val failure =
      for {
        description <- description
        throwable <- summon[Throwable]
      } yield Test.failure(throwable) ~ description

    val group = Gen.lzy(
      for {
        x <- summon[Test[Pure]]
        y <- summon[Test[Pure]]
      } yield x |+| y
    )

    val label = Gen.lzy(
      for {
        description <- description
        test <- summon[Test[Pure]]
      } yield Test.label(description, test)
    )

    val message = Gen.lzy(
      for {
        description <- description
        test <- summon[Test[Pure]]
      } yield Test.message(description, test)
    )

    val success = description.map(Test.success(_))

    val generator = Gen.oneOf(error, failure, group, label, message, success)

    Arbitrary(generator)
  }

  implicit val cogenTest: Cogen[Test[Pure]] =
    Cogen { (seed, test) =>
      test.fold[Pure, Seed](
        effect = (test: Test[Pure]) => Cogen.perturb(seed, test),
        error = Cogen.perturb(seed, _),
        failure = Cogen.perturb(seed, _),
        group = Cogen.perturb(seed, _),
        label = (description, test) => Cogen.perturb(seed, (description, test)),
        message =
          (description, test) => Cogen.perturb(seed, (description, test)),
        success = seed
      )
    }
}
