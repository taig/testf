package io.taig.testf

import cats.effect.{ContextShift, IO}
import io.taig.testf.internal.Contexts
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class TestApp {
  protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  def suite: IO[Assertion[Pure]]
}
