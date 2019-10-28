package io.taig.testf

import cats.implicits._
import cats.effect.{Blocker, ContextShift, IO}
import io.taig.testf.internal.Contexts

trait AutoTestDiscovery {
  protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  protected def blocker: Blocker = Contexts.blocker

  def auto: IO[Assertion[Pure]] = {
    val message = "No auto tests were discovered. " +
      "Did you forget the @AutoTests annotation?"

    IO.raiseError(new IllegalStateException(message))
  }

  def additional: IO[Assertion[Pure]] = IO.pure(Test.empty)

  final def all: IO[Assertion[Pure]] = auto |+| additional
}
