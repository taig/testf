package com.ayendo.testf.runner

import cats.effect.concurrent.MVar
import cats.effect.{Async, ContextShift, IO, Sync}
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Logging, Reflection}
import com.ayendo.testf.{Summary, TestF}
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
      .unsafeRunSync()

    Array.empty
  }

  def execute(eventHandler: EventHandler,
              loggers: Array[Logger],
              continuation: Array[Task] => Unit): Unit =
    TestFTask
      .execute[IO](task, classLoader, eventHandler, loggers.toList, lock, async)
      .unsafeRunAsync(_ => continuation(Array.empty))
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
      result <- {
        implicit val contextShift: ContextShift[IO] = async
        Async.liftIO(testF.suite.run)
      }
      _ <- lock.take *> log[F](loggers, name, result) <* lock.put(true)
      event = TestFEvent(task, result)
      _ <- F.delay(eventHandler.handle(event))
    } yield ()
  }

  def log[F[_]: Sync](loggers: List[Logger],
                      name: String,
                      summary: Summary): F[Unit] =
    loggers.traverse_ { logger =>
      val message = Formatter.summary(summary, logger.ansiCodesSupported())
      Logging.print[F](logger, name, Console.GREEN) *>
        Logging.print(logger, message, Console.GREEN)
    }
}
