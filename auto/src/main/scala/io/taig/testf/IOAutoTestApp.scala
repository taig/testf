package io.taig.testf

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO}

abstract class IOAutoTestApp extends IOTestApp with AutoTestDiscovery[IO] {
  final override def suite: IO[Assertion[Pure]] = {
    implicit val shift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    all.interpret[IO](Interpreter.effectIOParallel)
  }
}
