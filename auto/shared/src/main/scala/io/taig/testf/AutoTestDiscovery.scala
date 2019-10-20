package io.taig.testf

import cats.effect.{ContextShift, IO}
import io.taig.testf.internal.Contexts

trait AutoTestDiscovery {
  protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  def auto: IO[Assertion[Pure]] = {
    val message = "No auto tests were discovered. " +
      "Did you forget the @AutoTests annotation?"

    IO.raiseError(new IllegalStateException(message))
  }
}
