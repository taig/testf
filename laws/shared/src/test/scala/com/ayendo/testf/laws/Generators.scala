package com.ayendo.testf.laws

import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit val arbitraryTest: Arbitrary[Test[Pure]] = {
    val error = (summon[String], description).mapN(Test.error(_) ~ _)

    val failure = (summon[Throwable], description).mapN(Test.failure(_) ~ _)

    val group = Gen.lzy((summon[Test[Pure]], summon[Test[Pure]]).mapN(_ |+| _))

    val label = Gen.lzy((description, summon[Test[Pure]]).mapN(Test.label))

    val message = Gen.lzy((description, summon[Test[Pure]]).mapN(Test.message))

    val success = description.map(Test.success(_))

    val generator = Gen.oneOf(error, failure, group, label, message, success)

    Arbitrary(generator)
  }

  implicit val cogenTest: Cogen[Test[Pure]] =
    Cogen { (seed, test) =>
      test match {
        case effect: Test.Effect[Pure] => Cogen.perturb(seed, effect.test)
        case Test.Error(message)       => Cogen.perturb(seed, message)
        case Test.Failure(throwable)   => Cogen.perturb(seed, throwable)
        case Test.Group(tests)         => Cogen.perturb(seed, tests)
        case Test.Label(description, test) =>
          Cogen.perturb(seed, (description, test))
        case Test.Message(description, test) =>
          Cogen.perturb(seed, (description, test))
        case Test.Success => seed
      }
    }
}
