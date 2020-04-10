package io.taig.testf

import cats.effect.{ConcurrentEffect, ContextShift, IO}
import com.github.ghik.silencer.silent
import io.taig.testf.dsl._

@AutoTest
object AutoTestPlainTest extends AutoTestDiscovery[Pure] {
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

@silent
@AutoTest
final class AutoTestComplicatedClassTest[F[_], A](a: String) extends AutoTestDiscovery[F]

object AutoTestComplicatedClassTest

@AutoTest
final class AutoTestImplicitsTest[F[_]: ConcurrentEffect: ContextShift] extends AutoTestDiscovery[F]

object AutoTestImplicitsTest {
  def apply[F[_]: ConcurrentEffect: ContextShift]: AutoTestImplicitsTest[F] =
    new AutoTestImplicitsTest[F]()
}
