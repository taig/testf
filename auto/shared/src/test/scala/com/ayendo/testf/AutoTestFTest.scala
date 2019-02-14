package com.ayendo.testf

import cats.Id

@AutoTestF
object AutoTestFTest {
  val valTest: Test[Id, Unit] = Test.unit("val")

  def defTest: Test[Id, Unit] = Test.unit("val")
}
