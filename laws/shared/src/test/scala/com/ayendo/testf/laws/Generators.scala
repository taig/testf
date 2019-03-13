package com.ayendo.testf.laws

import cats.Id
import cats.implicits._
import com.ayendo.testf._
import com.ayendo.testf.implicits._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit val arbitraryTest: Arbitrary[Test[Id]] = {
    val error = (description, summon[String]).mapN(_ @@ Test.error(_))

    val failure = (description, summon[Throwable]).mapN(_ @@ Test.failure(_))

    val group = Gen.lzy((summon[Test[Id]], summon[Test[Id]]).mapN(_ |+| _))

    val label = Gen.lzy((description, summon[Test[Id]]).mapN(Test.label))

    val message = Gen.lzy((description, summon[Test[Id]]).mapN(Test.message))

    val success = description.map(Test.success(_))

    val generator = Gen.oneOf(error, failure, group, label, message, success)

    Arbitrary(generator)
  }

  implicit val cogenTest: Cogen[Test[Id]] =
    Cogen { (seed, test) =>
      test match {
        case Test.Defer(test)        => Cogen.perturb(seed, test)
        case Test.Error(message)     => Cogen.perturb(seed, message)
        case Test.Failure(throwable) => Cogen.perturb(seed, throwable)
        case Test.Group(tests)       => Cogen.perturb(seed, tests)
        case Test.Label(description, test) =>
          Cogen.perturb(seed, (description, test))
        case Test.Message(description, test) =>
          Cogen.perturb(seed, (description, test))
        case Test.Success => seed
      }
    }
}
