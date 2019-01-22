package com.ayendo.testf.runner

import cats.effect.{ContextShift, IO}
import sbt.testing._

final case class TestFRunner(args: Array[String],
                             remoteArgs: Array[String],
                             classLoader: ClassLoader,
                             async: ContextShift[IO])
    extends Runner {
  override def done(): String = ""

  override def tasks(list: Array[TaskDef]): Array[Task] =
    list.map(task => new TestFTask(task, classLoader, async))

  def receiveMessage(msg: String): Option[String] = None

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    new TestFTask(deserializer(task), classLoader, async)

}
