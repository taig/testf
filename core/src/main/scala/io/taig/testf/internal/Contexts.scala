package io.taig.testf.internal

import cats.effect.{Blocker, ContextShift, IO}

import scala.concurrent.ExecutionContext

object Contexts {
  val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  val blocker: Blocker = Blocker.liftExecutionContext(ExecutionContext.global)
}
