package com.ayendo.testf.runner

import cats.Id
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
      test <- {
        implicit val contextShift: ContextShift[IO] = async
        Async.liftIO(testF.suite.compile)
      }
      _ <- lock.take
      _ <- log[F](loggers, name, test)
      _ <- lock.put(true)
      events = test.root.map(TestFEvent(task, _))
      _ <- events.traverse_(event => F.delay(eventHandler.handle(event)))
    } yield ()
  }

  def log[F[_]: Sync](loggers: List[Logger],
                      name: String,
                      test: Test[Id, _]): F[Unit] =
    loggers.traverse_ { logger =>
      val color = logger.ansiCodesSupported()
      val title =
        Text.colorize(name, if (test.success) Console.GREEN else Console.RED)
      val message = Formatter.test(test, color)
      Logging.print(logger, title) *> Logging.print(logger, message)
    }
}
