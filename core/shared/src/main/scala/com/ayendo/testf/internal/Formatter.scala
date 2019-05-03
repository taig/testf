package com.ayendo.testf.internal

import cats.Id
import cats.implicits._
import com.ayendo.testf.Test

import scala.compat.Platform.EOL

object Formatter {
  val label: Test[Id] => String = {
    case Test.Defer(test)           => label(test)
    case Test.Error(_)              => "error"
    case Test.Failure(_)            => "failure"
    case Test.Label(description, _) => description
    case Test.Message(_, test)      => label(test)
    case Test.Group(tests) =>
      tests.map(label).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => tail.foldLeft(head)(_ + " |+| " + _)
      }
    case Test.Success => "success"
  }

  def test(value: Test[Id], duration: Option[Long], color: Boolean): String =
    this.test(color, duration, level = 0)(value)

  def test(
      color: Boolean,
      duration: Option[Long],
      level: Int
  ): Test[Id] => String = {
    case group @ Test.Group(tests) =>
      val label = this.label(group)
      if (group.success)
        success(label, this.duration(duration, level), color)
      else {
        val details =
          tests.map(this.test(color, duration, level + 1)).mkString(EOL)
        error(label, message = None, this.duration(duration, level), color) + EOL + Text
          .padLeft(details, 2)
      }
    case Test.Label(description, Test.Error(message)) =>
      error(description, Some(message), this.duration(duration, level), color)
    case Test.Label(description, Test.Failure(throwable)) =>
      failure(description, throwable, this.duration(duration, level), color)
    case Test.Label(description, group @ Test.Group(tests)) =>
      if (group.success) success(description, duration, color)
      else {
        val details = tests.map(this.test(color, duration, level)).mkString(EOL)
        error(
          description,
          message = None,
          this.duration(duration, level),
          color
        ) + EOL +
          Text.padLeft(details, 2)
      }
    case Test.Label(description, Test.Label(_, test)) =>
      this.test(color, duration, level)(Test.Label(description, test))
    case Test.Label(description, Test.Success) =>
      success(description, duration, color)
    case Test.Message(description, test) =>
      val details =
        if (test.success)
          Text.colorizeCond(description, Console.GREEN, color)
        else Text.colorizeCond(description, Console.RED, color)
      this.test(color, duration, level)(test) + EOL + Text.padLeft(
        details,
        level * 2
      )
    case Test.Success => success("success", None, color)
    case test         => show"Unknown format: $test"
  }

  def duration(value: Option[Long], level: Int): Option[Long] =
    if (level === 0) value else None

  def error(
      description: String,
      message: Option[String],
      duration: Option[Long],
      color: Boolean
  ): String = {
    val value = s"✗ $description" + duration.fold("")(" (" + _ + "ms)") +
      message.map(EOL + Text.padLeft(_, 2)).orEmpty
    Text.colorizeCond(value, Console.RED, color)
  }

  def failure(
      description: String,
      throwable: Throwable,
      duration: Option[Long],
      color: Boolean
  ): String = {
    val error = Formatter.throwable(throwable)
    val details = s"⚡$description" + duration.fold("")(" (" + _ + "ms)") + EOL + Text
      .padLeft(error, 2)
    Text.colorizeCond(details, Console.RED, color)
  }

  def skip(description: String, color: Boolean): String =
    Text.colorizeCond(s"~ $description", Console.YELLOW, color)

  def success(
      description: String,
      duration: Option[Long],
      color: Boolean
  ): String = {
    val details = s"✓ $description" + duration.fold("")(" (" + _ + "ms)")
    Text.colorizeCond(details, Console.GREEN, color)
  }

  def throwable(throwable: Throwable): String =
    throwable.getMessage + EOL + throwable.getStackTrace.mkString("", EOL, EOL)
}
