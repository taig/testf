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
    val error = summon[String].map(Test.error)

    val failure = summon[Throwable].map(Test.failure)

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

    val success = Gen.const(Test.success)

    val generator = Gen.oneOf(error, failure, group, label, message, success)

    Arbitrary(generator)
  }

  implicit val cogenTest: Cogen[Test[Pure]] = Cogen({
    case (seed, Test.And(tests))         => Cogen.perturb(seed, tests)
    case (seed, test: Test.Eval[Pure])   => Cogen.perturb(seed, test.test)
    case (seed, Test.Error(message))     => Cogen.perturb(seed, message)
    case (seed, Test.Failure(throwable)) => Cogen.perturb(seed, throwable)
    case (seed, Test.Label(description, test)) =>
      Cogen.perturb(seed, (description, test))
    case (seed, Test.Message(description, test)) =>
      Cogen.perturb(seed, (description, test))
    case (seed, Test.Not(test))  => Cogen.perturb(seed, test)
    case (seed, Test.Or(tests))  => Cogen.perturb(seed, tests)
    case (seed, Test.Skip(test)) => Cogen.perturb(seed, test)
    case (seed, Test.Success)    => seed
  }: (Seed, Test[Pure]) => Seed)
}
