package com.ayendo.testf

import cats.effect.IO
import com.ayendo.testf.dsl._

@AutoTestF
object AutoTestFTest {
  val valTest: Test[Pure] = success

  def defTest: Test[Pure] = success("val")

  val ioTest: Test[IO] = eval("IO val")(IO.pure(success))
}
