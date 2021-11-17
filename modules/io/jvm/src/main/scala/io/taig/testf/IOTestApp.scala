package io.taig.testf

import cats.effect.{IO, Outcome}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait IOTestApp extends TestApp:
  def runtime: IORuntime = IORuntime.global

  def suite: IO[Test[IO]]

  def runner: Runner[IO, IO] = ConcurrentRunner[IO]

  override def main(logger: String => Unit, callback: Option[Report] => Unit): Unit =
    suite
      .flatMap(runner.run)
      .flatTap(report => IO(logger(Formatter(report, colors = true))))
      .guaranteeCase {
        case Outcome.Succeeded(report) => report.flatMap(report => IO(callback(Some(report))))
        case Outcome.Errored(_)        => IO(callback(None))
        case Outcome.Canceled()        => IO(callback(None))
      }
      .unsafeRunSync()(runtime)
