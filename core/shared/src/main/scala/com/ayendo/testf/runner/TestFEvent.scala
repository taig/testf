package com.ayendo.testf.runner

import com.ayendo.testf.Test
import sbt.testing._

case class TestFEvent(task: TaskDef, result: Test.Result) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val duration: Long = result.duration.getOrElse(-1)

  override val status: Status = result.test.status

  override val throwable: OptionalThrowable =
    result.test.failure.fold(new OptionalThrowable())(new OptionalThrowable(_))
}
