package com.ayendo.testf

import cats.effect.{ContextShift, IO}
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

import scala.concurrent.ExecutionContext

@EnableReflectiveInstantiation
abstract class TestF {
  def suite: List[IO[Test]]

  implicit lazy val contextShift: ContextShift[IO] = TestF.defaultContextShift
}

object TestF {
  implicit val defaultContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)
}
