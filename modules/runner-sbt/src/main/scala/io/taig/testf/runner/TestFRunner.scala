package io.taig.testf.runner

import sbt.testing.*

final class TestFRunner(override val args: Array[String], override val remoteArgs: Array[String], loader: ClassLoader)
    extends Runner:
  override def done(): String = ""

  override def tasks(defs: Array[TaskDef]): Array[Task] = defs.map(new TestFTask(_, loader))

  def receiveMessage(msg: String): Option[String] = None

  def serializeTask(task: Task, serializer: TaskDef => String): String = serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: String => TaskDef): Task = new TestFTask(deserializer(task), loader)
