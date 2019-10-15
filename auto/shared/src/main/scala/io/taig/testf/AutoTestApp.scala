package io.taig.testf

import cats.effect.{ContextShift, IO}
import cats.implicits._

import scala.concurrent.ExecutionContext

abstract class AutoTestApp extends TestApp {
  def discover: List[IO[Assertion[Pure]]]

  override def suite: IO[Assertion[Pure]] = ???
}
