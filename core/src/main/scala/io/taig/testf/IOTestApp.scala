package io.taig.testf

import cats.effect.{Blocker, ContextShift, IO, Timer}
import io.taig.testf.internal.Contexts
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class IOTestApp {
  protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  protected implicit def timer: Timer[IO] = Contexts.timer

  protected def blocker: Blocker = Contexts.blocker

  def suite: IO[Assertion[Pure]]
}
