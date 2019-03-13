package com.ayendo.testf.syntax

import com.ayendo.testf.Test

final class TestFStringOps(val value: String) extends AnyVal {
  def @@[F[_]](test: Test[F]): Test[F] = Test.label(value, test)
}

trait TestFStringSyntax {
  implicit def testFStringSyntax(value: String): TestFStringOps =
    new TestFStringOps(value)
}
