package com.ayendo.testf

import cats.Id
import cats.effect.IO

@AutoTestF
object AutoTestFTest {
  val valTest: Test = Test.success("val")

  def defTest: Test = Test.success("val")

  val idTest: Id[Test] = Test.label("Id val", valTest)

  val ioTest: IO[Test] = Test.labelF("IO val", IO.pure(valTest))
}
