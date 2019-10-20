package io.taig.testf

import cats._
import cats.implicits._

trait Assertions {
  def assertion(predicate: Boolean, message: => String): Assertion[Pure] =
    Assertion(predicate, message)

  def equal[A: Eq: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(
      actual === expected,
      show"'$actual' is not equal to expected '$expected'"
    )

  def equalUniversal[A](expected: A)(actual: A): Assertion[Pure] =
    equal[A](expected)(actual)(Eq.fromUniversalEquals, Show.fromToString)

  def endsWith(ending: String)(actual: String): Assertion[Pure] =
    assertion(
      actual endsWith ending,
      show"'$actual' does not end with '$ending'"
    )

  def gt[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual > expected, s"'$actual' is not > '$expected'")

  def gte[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual >= expected, s"'$actual' is not >= '$expected'")

  def isEmpty[A: Monoid: Eq: Show](value: A): Assertion[Pure] =
    assertion(value.isEmpty, show"'$value' is not empty")

  def isEmptyK[F[_]: MonoidK, A](
      value: F[A]
  )(implicit eq: Eq[F[A]], show: Show[F[A]]): Assertion[Pure] =
    isEmpty[F[A]](value)(MonoidK[F].algebra, Eq[F[A]], Show[F[A]])

  def lt[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual < expected, s"'$actual' is not < '$expected'")

  def lte[A: PartialOrder: Show](expected: A)(actual: A): Assertion[Pure] =
    assertion(actual <= expected, s"'$actual' is not <= '$expected'")

  def notEmpty[A: Monoid: Eq: Show](value: A): Assertion[Pure] =
    assertion(!value.isEmpty, show"'$value' is empty")

  def startsWith(start: String)(actual: String): Assertion[Pure] =
    assertion(
      actual startsWith start,
      show"'$actual' does not start with '$start'"
    )
}
