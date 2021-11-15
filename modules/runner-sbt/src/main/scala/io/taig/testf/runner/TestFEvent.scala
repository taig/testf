package io.taig.testf.runner

import io.taig.testf.{Report, Result}
import sbt.testing.*

import scala.annotation.tailrec

final class TestFEvent(task: TaskDef, report: Report) extends Event:
  override def fullyQualifiedName(): String = task.fullyQualifiedName()

  override def fingerprint(): Fingerprint = task.fingerprint()

  override def selector(): Selector = task.selectors().head

  override def status(): Status = TestFEvent.status(report)

  override def throwable(): OptionalThrowable = report.failure.fold(new OptionalThrowable())(new OptionalThrowable(_))

  override def duration(): Long = -1

object TestFEvent:
  def status(report: Report): Status = report match
    case Report.Assertion(Result.Success)    => Status.Success
    case Report.Assertion(Result.Skipped)    => Status.Skipped
    case Report.Assertion(Result.Error(_))   => Status.Error
    case Report.Assertion(Result.Failure(_)) => Status.Failure
    case Report.Label(_, report)             => status(report)
    case Report.Group(reports)               => reports.map(status).max
