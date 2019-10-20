package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl._

@AutoTest
object AutoTestPlainTest extends AutoTestApp {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))

  def helper(value: String): Test[Pure, String] = pure(value)

  test("foobar")(helper("foobar"))
}

@AutoTest
object AutoTestInheritedTest extends AutoTestApp {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))
}
