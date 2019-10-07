package com.ayendo.testf

import cats.effect.IO

@AutoTestF
object AutoTestFTest {
  val valTest: Test[Pure] = Test.success

  def defTest: Test[Pure] = Test.success("val")

  val ioTest: Test[IO] =
    Test.label("IO val")(Test.eval[IO](IO.pure(Test.success)))
}
