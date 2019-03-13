package com.ayendo.testf

import cats.Id
import cats.effect.IO

@AutoTestF
object AutoTestFTest {
  val valTest: Test[Id] = Test.success

  def defTest: Test[Id] = Test.success("val")

  val ioTest: Test[IO] =
    Test.label("IO val", Test.defer[IO](IO.pure(Test.success)))
}
