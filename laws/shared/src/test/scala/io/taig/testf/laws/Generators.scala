package io.taig.testf.laws

import cats.implicits._
import io.taig.testf._
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  val test: Gen[Assertion[Pure]] = {
    val error = description.map(Test.error)

    val failure = Arbitrary.arbitrary[Throwable].map(Test.failure)

    val group = Gen.lzy(
      for {
        x <- test
        y <- test
      } yield x |+| y
    )

    val label = Gen.lzy(
      for {
        description <- description
        test <- test
      } yield Test.label(description, test)
    )

    val message = Gen.lzy(
      for {
        description <- description
        test <- test
      } yield Test.message(description, test)
    )

    val success = Gen.const(Test.unit)

    Gen.oneOf(error, failure, group, label, message, success)
  }
}
