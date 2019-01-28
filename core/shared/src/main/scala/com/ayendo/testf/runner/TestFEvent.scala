package com.ayendo.testf.runner

import com.ayendo.testf.Report
import sbt.testing._

case class TestFEvent(task: TaskDef, report: Report) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val status: Status = report.toStatus

  override val throwable: OptionalThrowable = report match {
    case Report.Failure(_, throwable) => new OptionalThrowable(throwable)
    case _                             => new OptionalThrowable()
  }

  override val duration: Long = -1 // TODO
}
