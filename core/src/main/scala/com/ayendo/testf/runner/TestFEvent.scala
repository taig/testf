package com.ayendo.testf.runner

import com.ayendo.testf.Summary
import sbt.testing._

case class TestFEvent(task: TaskDef, summary: Summary) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val status: Status = summary.toStatus

  override val throwable: OptionalThrowable = summary match {
    case Summary.Failure(_, throwable) => new OptionalThrowable(throwable)
    case _                             => new OptionalThrowable()
  }

  override val duration: Long = -1 // TODO
}
