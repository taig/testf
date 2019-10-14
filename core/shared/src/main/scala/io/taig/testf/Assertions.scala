package io.taig.testf

import cats._
import cats.implicits._

trait Assertions {
  def assert(predicate: Boolean, message: => String): Assertion =
    if (predicate) Test.unit else Test.error(message)

  def equal[A: Eq: Show](expected: A)(actual: A): Assertion =
    assert(
      actual === expected,
      show"'$actual' is not equal to expected '$expected'"
    )

  def equalUniversal[A](expected: A)(actual: A): Assertion =
    equal[A](expected)(actual)(Eq.fromUniversalEquals, Show.fromToString)

  def endsWith(ending: String)(actual: String): Assertion =
    assert(
      actual endsWith ending,
      show"'$actual' does not end with '$ending'"
    )

  def gt[A: PartialOrder: Show](expected: A)(actual: A): Assertion =
    assert(actual > expected, s"'$actual' is not > '$expected'")

  def gte[A: PartialOrder: Show](expected: A)(actual: A): Assertion =
    assert(actual >= expected, s"'$actual' is not >= '$expected'")

  def isEmpty[A: Monoid: Eq: Show](value: A): Assertion =
    assert(value.isEmpty, show"'$value' is not empty")

  def lt[A: PartialOrder: Show](expected: A)(actual: A): Assertion =
    assert(actual < expected, s"'$actual' is not < '$expected'")

  def lte[A: PartialOrder: Show](expected: A)(actual: A): Assertion =
    assert(actual <= expected, s"'$actual' is not <= '$expected'")

  def notEmpty[A: Monoid: Eq: Show](value: A): Assertion =
    assert(!value.isEmpty, show"'$value' is empty")

  def startsWith(start: String)(actual: String): Assertion =
    assert(
      actual startsWith start,
      show"'$actual' does not start with '$start'"
    )
}
