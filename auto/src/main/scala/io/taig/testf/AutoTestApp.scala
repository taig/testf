package io.taig.testf

import cats.effect.{ContextShift, IO}
import io.taig.testf.internal.Contexts

abstract class AutoTestApp extends TestApp with AutoTestDiscovery {
  override protected implicit def contextShit: ContextShift[IO] =
    Contexts.contextShift

  override def suite: IO[Assertion[Pure]] = {
    val name = getClass.getName.replace("$", "")
    auto.map(Test.label(name, _))
  }
}
