package com.ayendo.testf.internal

import cats.effect.Sync
import sbt.testing.Logger

object Logging {
  def print[F[_]](logger: Logger,
                  message: String,
                  color: String = Console.RESET)(implicit F: Sync[F]): F[Unit] =
    F.delay(logger.info(Text.colorize(message, color)))
}
