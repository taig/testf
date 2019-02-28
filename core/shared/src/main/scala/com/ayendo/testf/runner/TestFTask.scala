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
                      lock: MVar[IO, Boolean],
                      async: ContextShift[IO])
    extends Task {
  override def tags(): Array[String] = Array.empty

  override def taskDef(): TaskDef = task

  override def execute(eventHandler: EventHandler,
                       loggers: Array[Logger]): Array[Task] = {
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock, async)
      .handleError(recover(loggers))
      .unsafeRunSync()

    Array.empty
  }

  def execute(eventHandler: EventHandler,
              loggers: Array[Logger],
              continuation: Array[Task] => Unit): Unit =
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock, async)
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
                    lock: MVar[F, Boolean],
                    async: ContextShift[IO])(implicit F: Async[F]): F[Unit] = {
    for {
      name <- F.delay(task.fullyQualifiedName())
      module <- Reflection.loadModule[F](classLoader, name)
      testF <- F.delay(module.asInstanceOf[TestF])
      tests <- {
        implicit val contextShift: ContextShift[IO] = async

        val tests = testF.suite.map { test =>
          for {
            start <- IO(System.currentTimeMillis())
            value <- test.handleError(Test.failure)
            end <- IO(System.currentTimeMillis())
          } yield (end - start, value)
        }

        Async.liftIO(tests.parSequence)
      }
      _ <- lock.take
      _ <- log[F](loggers, name, tests)
      _ <- lock.put(true)
      events = tests.map {
        case (duration, test) => TestFEvent(task, duration, test)
      }
      _ <- events.traverse_(event => F.delay(eventHandler.handle(event)))
    } yield ()
  }

  def log[F[_]: Sync](loggers: List[Logger],
                      name: String,
                      tests: List[(Long, Test)]): F[Unit] =
    loggers.traverse_ { logger =>
      val color = logger.ansiCodesSupported()
      val success = tests.map(_._2).forall(_.success)
      val title =
        Text.colorize(name, if (success) Console.GREEN else Console.RED)
      val messages = tests.map {
        case (duration, test) => Formatter.test(test, duration, color)
      }

      Logging.print(logger, title) *> messages.traverse_(
        Logging.print(logger, _))
    }
}
