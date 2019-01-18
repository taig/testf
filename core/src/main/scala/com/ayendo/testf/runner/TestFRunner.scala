package com.ayendo.testf.runner

import cats.effect.{ContextShift, IO}
import sbt.testing._

final case class TestFRunner(args: Array[String],
                             remoteArgs: Array[String],
                             classLoader: ClassLoader,
                             single: ContextShift[IO],
                             async: ContextShift[IO])
    extends Runner {
  override def done(): String = ""

  override def tasks(list: Array[TaskDef]): Array[Task] =
    list.map(task => new TestFTask(task, classLoader, single, async))
}
