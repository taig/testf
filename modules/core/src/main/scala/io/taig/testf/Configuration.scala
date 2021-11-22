package io.taig.testf

import scala.concurrent.duration.Duration

final case class Configuration(timeout: Duration, skip: Boolean)
