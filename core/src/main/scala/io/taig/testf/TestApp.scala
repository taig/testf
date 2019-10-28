package io.taig.testf

import cats.effect.{Blocker, ContextShift, IO}
import io.taig.testf.internal.Contexts
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class TestApp {
  protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  protected def blocker: Blocker = Contexts.blocker

  def suite: IO[Assertion[Pure]]
}
