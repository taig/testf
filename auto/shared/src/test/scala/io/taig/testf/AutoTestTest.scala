package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl._

@AutoTest
object AutoTestPlainTest {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))
}

@AutoTest
object AutoTestInheritedTest extends AutoTestApp {
  test("foo")(pure(42))

  force("bar")(IO.pure(unit))
}
