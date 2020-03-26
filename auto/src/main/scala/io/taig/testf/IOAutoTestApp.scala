package io.taig.testf

import cats.effect.IO

abstract class IOAutoTestApp extends IOTestApp with AutoTestDiscovery[IO] {
  final override def suite: IO[Assertion[Pure]] = all.interpret
}
