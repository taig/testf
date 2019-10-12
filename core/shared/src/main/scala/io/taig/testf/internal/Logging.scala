package io.taig.testf.internal

import cats.effect.Sync
import sbt.testing.Logger

object Logging {
  def print[F[_]](logger: Logger, message: String)(
      implicit F: Sync[F]
  ): F[Unit] = F.delay(logger.info(message))
}
