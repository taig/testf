package io.taig.testf

import scala.concurrent.duration.*

final case class Configuration(timeout: Duration, skip: Boolean)

object Configuration:
  val Default: Configuration = Configuration(timeout = 30.seconds, skip = false)
