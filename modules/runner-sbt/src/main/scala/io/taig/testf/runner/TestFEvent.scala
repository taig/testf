package io.taig.testf.runner

import io.taig.testf.{Evaluation, Result}
import sbt.testing.*

import scala.annotation.tailrec

final class TestFEvent(task: TaskDef, report: Evaluation) extends Event:
  override def fullyQualifiedName(): String = task.fullyQualifiedName()

  override def fingerprint(): Fingerprint = task.fingerprint()

  override def selector(): Selector = task.selectors().head

  override def status(): Status = TestFEvent.status(report)

  override def throwable(): OptionalThrowable = report.summary match
    case Result.Failure(throwable) => new OptionalThrowable(throwable)
    case _                         => new OptionalThrowable()

  override def duration(): Long = -1

object TestFEvent:
  def status(report: Evaluation): Status = report match
    case Evaluation.Assertion(Result.Success)    => Status.Success
    case Evaluation.Assertion(Result.Skipped)    => Status.Skipped
    case Evaluation.Assertion(Result.Error(_))   => Status.Error
    case Evaluation.Assertion(Result.Failure(_)) => Status.Failure
    case Evaluation.Label(_, report)             => status(report)
    case Evaluation.Group(reports)               => reports.map(status).max
