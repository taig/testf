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

  implicit def arbitraryTest: Arbitrary[Test] = {
    val error = (description, summon[String]).mapN(Test.error)

    val failure = (description, summon[Throwable]).mapN(Test.failure)

    val group = Gen.lzy((summon[Test], summon[Test]).mapN(_ |+| _))

    val label = Gen.lzy((description, summon[Test]).mapN(Test.label))

    val message = Gen.lzy((description, summon[Test]).mapN(Test.message))

    val skip = Gen.lzy(summon[Test]).map(Test.skip)

    val success = description.map(Test.success(_))

    val generator =
      Gen.oneOf(error, failure, group, label, message, skip, success)

    Arbitrary(generator)
  }

  implicit def cogenTest[A]: Cogen[Test] =
    Cogen({
      case Test.Error(_)      => 1
      case Test.Failure(_)    => 2
      case Test.Group(_)      => 3
      case Test.Label(_, _)   => 4
      case Test.Message(_, _) => 5
      case Test.Skip(_)       => 6
      case Test.Success       => 7
    }: Test => Long)
}
