package io.taig.testf

import cats.effect.IO
import cats.implicits._
import io.taig.testf.dsl._

@AutoTest
object AutoTestPlainTest {
  test("foo")(pure(42))

  test("bar")(isEqual(1)(1))
}

@AutoTest
object AutoTestAppTest extends IOAutoTestApp {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))

  def helper(value: String): Test[Pure, String] = pure(value)

  test("foobar")(helper("foobar"))
}

@AutoTest
object AutoTestInheritedTest extends AutoTestDiscovery[IO] {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))
}

@AutoTest
final class AutoTestClassTest extends AutoTestDiscovery[IO]

@AutoTest
final class AutoTestComplicatedClassTest[F[_], A](a: String)
    extends AutoTestDiscovery[F]

object AutoTestComplicatedClassTest
