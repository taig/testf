package io.taig.testf

import cats._
import cats.implicits._

trait Assertions {
  def assertion(predicate: Boolean, message: => String): Assertion[Pure] =
    Assertion(predicate, message)

  def endsWith(ending: String)(actual: String): Assertion[Pure] =
    assertion(
      actual endsWith ending,
      show"'$actual' does not end with '$ending'"
    )

  def isEqual[A: Eq: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(
      actual === expected,
      show"'$actual' is not equal to expected '$expected'"
    )

  def isEqualUniversal[A](expected: A)(actual: A): Assertion[Pure] =
    isEqual[A](expected)(actual)(Eq.fromUniversalEquals, Show.fromToString)

  def isGt[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual > expected, s"'$actual' is not > '$expected'")

  def isGte[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual >= expected, s"'$actual' is not >= '$expected'")

  def isEmpty[A: Monoid: Eq: Show](value: A): Assertion[Pure] =
    assertion(value.isEmpty, show"'$value' is not empty")

  def isEmptyK[F[_]: MonoidK, A](
      value: F[A]
  )(implicit eq: Eq[F[A]], show: Show[F[A]]): Assertion[Pure] =
    isEmpty[F[A]](value)(MonoidK[F].algebra, Eq[F[A]], Show[F[A]])

  def isLt[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual < expected, s"'$actual' is not < '$expected'")

  def isLte[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual <= expected, s"'$actual' is not <= '$expected'")

  def isNotEmpty[A: Monoid: Eq: Show](value: A): Assertion[Pure] =
    assertion(!value.isEmpty, show"'$value' is empty")

  def isTrue(value: Boolean): Assertion[Pure] =
    assertion(value, "false")

  def isFalse(value: Boolean): Assertion[Pure] =
    assertion(!value, "true")

  def startsWith(start: String)(actual: String): Assertion[Pure] =
    assertion(
      actual startsWith start,
      show"'$actual' does not start with '$start'"
    )
}
