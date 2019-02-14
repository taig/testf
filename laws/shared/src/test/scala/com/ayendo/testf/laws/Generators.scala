package com.ayendo.testf.laws

import cats.Id
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit def arbitraryTest[A: Arbitrary]: Arbitrary[Test[Id, A]] = {
    val error = (description, summon[String]).mapN(Test.error[Id])

    val failure = (description, summon[Throwable]).mapN(Test.failure[Id])

    val group =
      Gen.lzy((summon[Test[Id, A]], summon[Test[Id, A]]).mapN(_ |+| _))

    val label =
      Gen.lzy((description, summon[Test[Id, A]]).mapN(Test.label[Id, A]))

    val message =
      Gen.lzy((description, summon[Test[Id, A]]).mapN(Test.message[Id, A]))

    val skip = Gen.lzy(summon[Test[Id, A]].map(Test.skip[Id, A]))

    val success = (description, summon[A]).mapN(Test.success[Id, A])

    val generator =
      Gen.oneOf(error, failure, group, label, message, skip, success)

    Arbitrary(generator)
  }

  implicit def cogenTest[A]: Cogen[Test[Id, A]] =
    Cogen({
      case Test.Defer(_)      => 0
      case Test.Error(_)      => 1
      case Test.Failure(_)    => 2
      case Test.Group(_)      => 3
      case Test.Label(_, _)   => 4
      case Test.Message(_, _) => 5
      case Test.Skip(_)       => 6
      case Test.Success(_)    => 7
    }: Test[Id, A] => Long)
}
