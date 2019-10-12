package io.taig.testf

import cats._
import cats.implicits._

trait Assertion {
  def assert(predicate: Boolean, message: => String): Test[Pure] =
    if (predicate) Test.success else Test.error(message)

  def equal[A: Eq: Show](expected: A)(actual: A): Test[Pure] =
    assert(
      actual === expected,
      show"'$actual' is not equal to expected '$expected'"
    )

  def equalUniversal[A](expected: A)(actual: A): Test[Pure] =
    equal[A](expected)(actual)(Eq.fromUniversalEquals, Show.fromToString)

  def endsWith(ending: String)(actual: String): Test[Pure] =
    assert(
      actual endsWith ending,
      show"'$actual' does not end with '$ending'"
    )

  def gt[A: PartialOrder: Show](expected: A)(actual: A): Test[Pure] =
    assert(actual > expected, s"'$actual' is not > '$expected'")

  def gte[A: PartialOrder: Show](expected: A)(actual: A): Test[Pure] =
    assert(actual >= expected, s"'$actual' is not >= '$expected'")

  def isEmpty[A: Monoid: Eq: Show](value: A): Test[Pure] =
    assert(value.isEmpty, show"'$value' is not empty")

  def lt[A: PartialOrder: Show](expected: A)(actual: A): Test[Pure] =
    assert(actual < expected, s"'$actual' is not < '$expected'")

  def lte[A: PartialOrder: Show](expected: A)(actual: A): Test[Pure] =
    assert(actual <= expected, s"'$actual' is not <= '$expected'")

  def notEmpty[A: Monoid: Eq: Show](value: A): Test[Pure] =
    assert(!value.isEmpty, show"'$value' is empty")

  def startsWith(start: String)(actual: String): Test[Pure] =
    assert(
      actual startsWith start,
      show"'$actual' does not start with '$start'"
    )

  // TODO are those useful?
  def when[F[_]](condition: Boolean)(test: Test[F]): Test[F] =
    if (condition) test else Test.empty

  def unless[F[_]](condition: Boolean)(test: Test[F]): Test[F] =
    when(!condition)(test)
}

object Assertion extends Assertion
