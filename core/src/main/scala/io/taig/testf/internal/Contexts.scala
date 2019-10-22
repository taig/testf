package io.taig.testf.internal

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

object Contexts {
  val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)
}
