package io.taig.testf

import cats.effect.IO

abstract class IOAutoTestApp extends IOTestApp with AutoTestDiscovery[IO] {
  override final def suite: IO[Assertion[Pure]] = all.interpret
}
