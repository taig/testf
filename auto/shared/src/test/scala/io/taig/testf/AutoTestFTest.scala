package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl._

@AutoTestF
object AutoTestFTest {
  val valTest: Test[Pure, Unit] = unit

  def defTest: Test[Pure, Unit] = success("val")

  val ioTest: Test[IO, Unit] = force("IO val")(IO.pure(unit))
}
