package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl._

@AutoTest
object AutoTestPlainTest {
  test("foo")(unit)

  force("bar")(IO.pure(unit))
}

@AutoTest
object AutoTestInheritedTest {}
