package io.taig.testf.runner

import io.taig.testf.TestApp
import sbt.testing.*

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import org.portablescala.reflect.{LoadableModuleClass, Reflect}

final class TestFTask(override val taskDef: TaskDef, loader: ClassLoader) extends Task:
  override val tags: Array[String] = Array.empty

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: Array[Task] => Unit): Unit =
    try {
      val name = taskDef.fullyQualifiedName()
      val logger: String => Unit = message => loggers.foreach(_.info(message))
      val module = loadModule(name).asInstanceOf[TestApp]
      module.main(
        logger,
        {
          case Some(report) =>
            val event = new TestFEvent(taskDef, report)
            eventHandler.handle(event)
            continuation(Array.empty)
          case None => continuation(Array.empty)
        }
      )
    } catch {
      case throwable =>
        throwable.printStackTrace()
        continuation(Array.empty)
    }

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] =
    val promise = Promise[Unit]()
    execute(eventHandler, loggers, _ => promise.success(()))
    Await.result(promise.future, Duration.Inf)
    Array.empty

  def loadModule(name: String): Any = {
    Reflect
      .lookupLoadableModuleClass(name + "$", loader)
      .getOrElse(new ClassNotFoundException(name))
      .asInstanceOf[LoadableModuleClass]
      .loadModule()
  }
