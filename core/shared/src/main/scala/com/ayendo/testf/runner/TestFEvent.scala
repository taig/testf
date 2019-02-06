package com.ayendo.testf.runner

import com.ayendo.testf.Test
import sbt.testing._

case class TestFEvent(task: TaskDef, test: Test[_]) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val status: Status = test.status

  override val throwable: OptionalThrowable =
    test.failure.fold(new OptionalThrowable())(new OptionalThrowable(_))

  override val duration: Long = -1 // TODO
}
