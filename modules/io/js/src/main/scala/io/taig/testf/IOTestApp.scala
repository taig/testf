package io.taig.testf

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, Outcome, Spawn}

import scala.concurrent.{Await, CancellationException}
import scala.concurrent.duration.*
import scala.scalajs.js

trait IOTestApp extends TestApp {
  def runtime: IORuntime = IORuntime.global

  def suite: IO[Test[IO]]

  def runner: Runner[IO, IO] = ConcurrentRunner[IO]

  override def main(logger: String => Unit, callback: Option[Report] => Unit): Unit =
    lazy val keepAlive: IO[Nothing] =
      IO.sleep(1.hour) >> keepAlive

    val run = suite.flatMap(runner.run).flatTap(report => IO(logger(Formatter(report, colors = true))))

    Spawn[IO]
      .raceOutcome(run, keepAlive)
      .flatMap {
        case Left(Outcome.Canceled()) =>
          IO.raiseError(new CancellationException("IOTestApp main fiber was canceled"))
        case Left(Outcome.Errored(throwable))  => IO.raiseError(throwable)
        case Left(Outcome.Succeeded(report))   => report
        case Right(Outcome.Errored(throwable)) => IO.raiseError(throwable)
        case Right(_)                          => sys.error("impossible")
      }
      .unsafeRunAsync({
        case Left(throwable) =>
          callback(None)

          throwable match {
            case _: CancellationException => reportExitCode(ExitCode.Error)
            case _: Throwable             => throw throwable
          }
        case Right(report) =>
          callback(Some(report))
          reportExitCode(ExitCode.Success)
      })(runtime)

  private[this] def reportExitCode(code: ExitCode): Unit =
    if (js.typeOf(js.Dynamic.global.process) != "undefined") {
      js.Dynamic.global.process.exitCode = code.code
    }
}
