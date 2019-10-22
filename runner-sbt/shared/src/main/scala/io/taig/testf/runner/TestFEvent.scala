package io.taig.testf.runner

import cats.Id
import io.taig.testf.{Status, _}
import io.taig.testf.internal.Tests
import sbt.testing.{Status => SbtStatus, _}

case class TestFEvent(task: TaskDef, test: Test[Id, Unit]) extends Event {
  override val fullyQualifiedName: String = task.fullyQualifiedName()

  override val fingerprint: Fingerprint = task.fingerprint()

  override val selector: Selector = task.selectors().head

  override val duration: Long = -1

  override val status: SbtStatus =
    Status.of(test) match {
      case Status.Error   => SbtStatus.Error
      case Status.Failure => SbtStatus.Failure
      case Status.Skip    => SbtStatus.Skipped
      case Status.Success => SbtStatus.Success
    }

  override val throwable: OptionalThrowable =
    Tests
      .throwable(test)
      .fold(new OptionalThrowable())(new OptionalThrowable(_))
}
