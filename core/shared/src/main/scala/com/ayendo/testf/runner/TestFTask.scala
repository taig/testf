package com.ayendo.testf.runner

import java.io.{PrintWriter, StringWriter}

import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import com.ayendo.testf._
import com.ayendo.testf.internal._
import sbt.testing._

final class TestFTask(task: TaskDef,
                      classLoader: ClassLoader,
                      lock: MVar[IO, Boolean])
    extends Task {
  override def tags(): Array[String] = Array.empty

  override def taskDef(): TaskDef = task

  override def execute(eventHandler: EventHandler,
                       loggers: Array[Logger]): Array[Task] = {
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock)
      .handleError(recover(loggers))
      .unsafeRunSync()

    Array.empty
  }

  def execute(eventHandler: EventHandler,
              loggers: Array[Logger],
              continuation: Array[Task] => Unit): Unit =
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock)
      .handleError(recover(loggers))
      .unsafeRunAsync(_ => continuation(Array.empty))

  def recover(loggers: Array[Logger])(throwable: Throwable): Unit =
    loggers.foreach { logger =>
      val writer = new StringWriter()
      throwable.printStackTrace(new PrintWriter(writer))

      logger.error(s"Failed to run test suite ${task.fullyQualifiedName()}")
      logger.error(writer.toString)
    }
}

object TestFTask {
  def execute[F[_]](task: TaskDef,
                    classLoader: ClassLoader,
                    eventHandler: EventHandler,
                    loggers: List[Logger],
                    lock: MVar[F, Boolean])(implicit F: Async[F]): F[Unit] = {
    for {
      name <- F.delay(task.fullyQualifiedName())
      module <- Reflection.loadModule[F](classLoader, name)
      testF <- F.delay(module.asInstanceOf[TestF])
      results <- Async.liftIO(testF.suite)
      _ <- lock.take
      _ <- log[F](loggers, name, results)
      _ <- lock.put(true)
      events = results.map(TestFEvent(task, _))
      _ <- events.traverse_(event => F.delay(eventHandler.handle(event)))
    } yield ()
  }

  def log[F[_]: Sync](loggers: List[Logger],
                      name: String,
                      results: List[Test.Result]): F[Unit] =
    loggers.traverse_ { logger =>
      val color = logger.ansiCodesSupported()
      val success = results.map(_.test).forall(_.success)
      val title =
        Text.colorize(name, if (success) Console.GREEN else Console.RED)
      val messages = results.map { result =>
        Formatter.test(result.test, result.duration, color)
      }

      Logging.print(logger, title) *> messages.traverse_(
        Logging.print(logger, _))
    }
}
