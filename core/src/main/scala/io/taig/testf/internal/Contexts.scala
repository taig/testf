package io.taig.testf.internal

import cats.effect.{Blocker, ContextShift, IO, Timer}

import scala.concurrent.ExecutionContext

object Contexts {
  val blocker: Blocker = Blocker.liftExecutionContext(ExecutionContext.global)

  val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val timer: Timer[IO] = IO.timer(ExecutionContext.global)
}
