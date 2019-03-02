package com.ayendo.testf.syntax

import cats.ApplicativeError
import com.ayendo.testf.Test

final class TestFStringOps(val value: String) extends AnyVal {
  def @@(test: Test): Test = Test.label(value, test)

  def @@[F[_]: ApplicativeError[?[_], Throwable]](test: F[Test]): F[Test] =
    Test.labelF(value, test)
}

trait TestFStringSyntax {
  implicit def testFStringSyntax(value: String): TestFStringOps =
    new TestFStringOps(value)
}
