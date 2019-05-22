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
  val status: Test[Pure] => Status = {
    case effect: Test.Effect[Pure] => status(effect.test)
    case _: Test.Error             => Status.Error
    case _: Test.Failure           => Status.Failure
    case group: Test.Group[Pure] =>
      group.tests.foldLeft(Status.Success) {
        case (Status.Error, _)   => Status.Error
        case (Status.Failure, _) => Status.Failure
        case (_, test)           => status(test)
      }
    case Test.Label(_, test)   => status(test)
    case Test.Message(_, test) => status(test)
    case Test.Success          => Status.Success
  }
}
