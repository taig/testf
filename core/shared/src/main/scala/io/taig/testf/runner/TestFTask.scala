package io.taig.testf.runner

import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import io.taig.testf._
import io.taig.testf.internal.{Formatter, Logging, Reflection}
import sbt.testing._

final class TestFTask(
    task: TaskDef,
    classLoader: ClassLoader,
    lock: MVar[IO, Boolean]
) extends Task {
  override def tags(): Array[String] = Array.empty

  override def taskDef(): TaskDef = task

  override def execute(
      eventHandler: EventHandler,
      loggers: Array[Logger]
  ): Array[Task] = {
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock)
      .handleError(recover(loggers))
      .unsafeRunSync()

    Array.empty
  }

  def execute(
      eventHandler: EventHandler,
      loggers: Array[Logger],
      continuation: Array[Task] => Unit
  ): Unit =
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock)
      .handleError(recover(loggers))
      .unsafeRunAsync(_ => continuation(Array.empty))

  def recover(loggers: Array[Logger])(throwable: Throwable): Unit =
    loggers.foreach { logger =>
      logger.error(s"Failed to run test suite ${task.fullyQualifiedName()}")
      logger.trace(throwable)
    }
}

object TestFTask {
  def execute[F[_]](
      task: TaskDef,
      classLoader: ClassLoader,
      eventHandler: EventHandler,
      loggers: List[Logger],
      lock: MVar[F, Boolean]
  )(implicit F: Async[F]): F[Unit] =
    for {
      name <- F.delay(task.fullyQualifiedName())
      module <- Reflection.loadModule[F](classLoader, name)
      testF <- F.delay(module.asInstanceOf[TestF])
      test <- Async.liftIO(testF.suite)
      _ <- lock.take
      _ <- log[F](loggers, test)
      _ <- lock.put(true)
      events = test.children.map(TestFEvent(task, _))
      _ <- events.traverse_(event => F.delay(eventHandler.handle(event)))
    } yield ()

  def log[F[_]: Sync](
      loggers: List[Logger],
      test: Test[Pure]
  ): F[Unit] =
    loggers.traverse_ { logger =>
      val color = logger.ansiCodesSupported()
      val message = Formatter.test(test, color)
      Logging.print(logger, message)
    }
}
