package com.ayendo.testf

import cats._
import cats.implicits._

trait Assertion {
  def equal[A: Eq: Show](value: A, expected: A): Test[Pure] =
    Test.assert(
      value === expected,
      show"'$value' is not equal to expected '$expected'"
    )

  def gt[A: PartialOrder: Show](value: A, expected: A): Test[Pure] =
    Test.assert(value > expected, s"'$value' is not > '$expected'")

  def gte[A: PartialOrder: Show](value: A, expected: A): Test[Pure] =
    Test.assert(value >= expected, s"'$value' is not >= '$expected'")

  def isEmpty[A: Monoid: Eq: Show](value: A): Test[Pure] =
    Test.assert(value.isEmpty, show"'$value' is not empty")

  def lt[A: PartialOrder: Show](value: A, expected: A): Test[Pure] =
    Test.assert(value < expected, s"'$value' is not < '$expected'")

  def lte[A: PartialOrder: Show](value: A, expected: A): Test[Pure] =
    Test.assert(value <= expected, s"'$value' is not <= '$expected'")

  def notEmpty[A: Monoid: Eq: Show](value: A): Test[Pure] =
    Test.assert(!value.isEmpty, show"'$value' is empty")

  def startsWith(value: String, expected: String): Test[Pure] =
    Test.assert(
      value startsWith expected,
      show"'$value' does not start with '$expected'"
    )
}

object Assertion extends Assertion
