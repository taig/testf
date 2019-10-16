package io.taig.testf

import cats.effect.IO

abstract class AutoTestApp extends TestApp {
  def auto: IO[Assertion[Pure]] = {
    val message = "No auto tests were discovered. " +
      "Did you forget the @AutoTest annotation?"

    IO.raiseError(new IllegalStateException(message))
  }

  override def suite: IO[Assertion[Pure]] = {
    val name = getClass.getName.replace("$", "")
    auto.map(Test.label(name, _))
  }
}
