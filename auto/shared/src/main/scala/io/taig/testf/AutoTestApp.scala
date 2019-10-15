package io.taig.testf

import cats.effect.IO

abstract class AutoTestApp extends TestApp {
  def discover: IO[Assertion[Pure]] = {
    val message = "No tests were discovered. " +
      "Did you forget the @AutoTest annotation?"

    IO.raiseError(new IllegalStateException(message))
  }

  override def suite: IO[Assertion[Pure]] = discover
}
