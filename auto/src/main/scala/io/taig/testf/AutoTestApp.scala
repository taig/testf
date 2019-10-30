package io.taig.testf

import cats.effect.{Blocker, ContextShift, IO}
import io.taig.testf.internal.Contexts

abstract class AutoTestApp extends IOTestApp with AutoTestDiscovery {
  override protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  override protected def blocker: Blocker = Contexts.blocker

  override def suite: IO[Assertion[Pure]] = {
    val name = getClass.getName.replace("$", "")
    all.map(Test.label(name, _))
  }
}
