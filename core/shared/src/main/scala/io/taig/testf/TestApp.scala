package io.taig.testf

import cats.effect.{ContextShift, IO}
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

import scala.concurrent.ExecutionContext

@EnableReflectiveInstantiation
abstract class TestApp {
  protected implicit def contextShit: ContextShift[IO] =
    TestApp.defaultContextShift

  def suite: IO[Assertion[Pure]]
}

object TestApp {
  val defaultContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)
}
