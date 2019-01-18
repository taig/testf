package com.ayendo.testf

import cats.effect.IO

abstract class TestF {
  def suite: List[IO[Summary]]
}
