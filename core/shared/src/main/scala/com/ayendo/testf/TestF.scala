package com.ayendo.testf

import cats.Parallel
import cats.effect.{ContextShift, IO}
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

import scala.concurrent.ExecutionContext

@EnableReflectiveInstantiation
abstract class TestF {
  def suite: List[IO[Test]]

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val parallel: Parallel[IO, IO.Par] = IO.ioParallel
}
