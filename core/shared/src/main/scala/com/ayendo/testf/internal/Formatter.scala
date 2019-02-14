package com.ayendo.testf.internal

import cats.Id
import cats.implicits._
import com.ayendo.testf.Test

import scala.compat.Platform.EOL

object Formatter {
  val label: Test[Id, _] => String = {
    case Test.Defer(test)      => label(test)
    case Test.Error(_)         => "error"
    case Test.Failure(_)       => "failure"
    case Test.Label(label, _)  => label
    case Test.Message(_, test) => label(test)
    case Test.Group(tests) =>
      tests.map(label).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => tail.foldLeft(head)(_ + " |+| " + _)
      }
    case Test.Skip(_)    => "skip"
    case Test.Success(_) => "success"
  }

  def test[A](value: Test[Id, A], color: Boolean): String =
    this.test(color, level = 0)(value)

  def test[A](color: Boolean, level: Int): Test[Id, A] => String = {
    case Test.Defer(test) => this.test(color, level)(test)
    case Test.Group(tests) if level === 0 =>
      tests.map(this.test(color, level + 1)).mkString(EOL)
    case group @ Test.Group(tests) =>
      val label = this.label(group)
      if (group.success) success(label, color)
      else {
        val details = tests.map(this.test(color, level)).mkString(EOL)
        error(label, message = None, color) + EOL + Text.padLeft(details, 2)
      }
    case Test.Label(description, Test.Error(message)) =>
      error(description, Some(message), color)
    case Test.Label(description, Test.Failure(throwable)) =>
      failure(description, throwable, color)
    case Test.Label(description, group @ Test.Group(tests)) =>
      if (group.success) success(description, color)
      else {
        val details = tests.map(this.test(color, level)).mkString(EOL)
        error(description, message = None, color) + EOL + Text.padLeft(details,
                                                                       2)
      }
    case Test.Label(description, Test.Label(_, test)) =>
      this.test(color, level)(Test.Label(description, test))
    case Test.Label(description, Test.Success(_)) => success(description, color)
    case Test.Message(description, test) =>
      val details =
        if (test.success) Text.colorizeCond(description, Console.GREEN, color)
        else Text.colorizeCond(description, Console.RED, color)
      this.test(color, level)(test) + EOL + Text.padLeft(details, level * 2)
    case Test.Skip(_)    => skip("skip", color)
    case Test.Success(_) => success("success", color)
    case test            => show"Unknown format: $test"
  }

  def error(description: String,
            message: Option[String],
            color: Boolean): String = {
    val value = s"✗ $description" +
      message.map(EOL + Text.padLeft(_, 2)).orEmpty
    Text.colorizeCond(value, Console.RED, color)
  }

  def failure(description: String,
              throwable: Throwable,
              color: Boolean): String = {
    val error = Formatter.throwable(throwable)
    val value = s"⚡$description" + EOL + Text.padLeft(error, 2)
    Text.colorizeCond(value, Console.RED, color)
  }

  def skip(description: String, color: Boolean): String =
    Text.colorizeCond(s"~ $description", Console.YELLOW, color)

  def success(description: String, color: Boolean): String =
    Text.colorizeCond(s"✓ $description", Console.GREEN, color)

  def throwable(throwable: Throwable): String =
    throwable.getMessage + EOL + throwable.getStackTrace.mkString("", EOL, EOL)
}
