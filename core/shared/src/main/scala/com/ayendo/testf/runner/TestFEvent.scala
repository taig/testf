package com.ayendo.testf.runner

import com.ayendo.testf._
import sbt.testing._

case class TestFEvent(task: TaskDef, test: Test[Pure]) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val duration: Long = -1

  override val status: Status = TestFEvent.status(test)

  override val throwable: OptionalThrowable =
    test.throwable.fold(new OptionalThrowable())(new OptionalThrowable(_))
}

object TestFEvent {
  val status: Test[Pure] => Status = _.fold[Pure, Status](
    effect = status,
    error = _ => Status.Error,
    failure = _ => Status.Failure,
    group = _.foldLeft(Status.Success) {
      case (Status.Error, _)   => Status.Error
      case (Status.Failure, _) => Status.Failure
      case (_, test)           => status(test)
    },
    label = (_, test) => status(test),
    message = (_, test) => status(test),
    success = Status.Success
  )
}
