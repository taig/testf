package com.ayendo.testf.internal

import java.io.{PrintWriter, StringWriter}

import com.ayendo.testf.{Pure, Test}

import scala.compat.Platform.EOL

object Formatter {
  val label: Test[Pure] => String = {
    case effect: Test.Effect[Pure]  => label(effect.test)
    case Test.Error(_)              => "error"
    case Test.Failure(_)            => "failure"
    case Test.Label(description, _) => description
    case Test.Message(_, test)      => label(test)
    case Test.Group(tests) =>
      tests.map(label).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => "(" + tail.foldLeft(head)(_ + ", " + _) + ")"
      }
    case Test.Success => "success"
  }

  def test(value: Test[Pure], color: Boolean): String =
    this.test(color, level = 0)(value)

  def test(
      color: Boolean,
      level: Int
  ): Test[Pure] => String = {
    case Test.Error(message)     => error("error", Some(message), color)
    case Test.Failure(throwable) => failure("failure", throwable, color)
    case Test.Group(tests) =>
      val messages = tests.map(test(_, color)).mkString(EOL)
      Text.padLeft(messages, level * 2)
    case Test.Success => success("success", color)
    case Test.Label(description, Test.Error(message)) =>
      error(description, Some(message), color)
    case Test.Label(description, Test.Failure(throwable)) =>
      failure(description, throwable, color)
    case Test.Label(description, Test.Success) => success(description, color)
    case Test.Label(description, group: Test.Group[Pure]) =>
      val label =
        if (group.success) success(description, color)
        else error(description, message = None, color)
      val details = test(color, level + 1)(group)
      label + EOL + details
    case Test.Label(description1, label: Test.Label[Pure]) =>
      test(color, level)(Test.label(description1, Test.of(label)))
    case Test.Label(label, Test.Message(message, test)) =>
      this.test(color, level)(
        Test.label(label + EOL + Text.padLeft(message, 2), test)
      )
    case test => s"No format for Test $test"
  }

  def error(
      description: String,
      message: Option[String],
      color: Boolean
  ): String = {
    val value = s"✗ $description" + message
      .map(EOL + Text.padLeft(_, 2))
      .getOrElse("")
    Text.colorizeCond(value, Console.RED, color)
  }

  def failure(
      description: String,
      throwable: Throwable,
      color: Boolean
  ): String = {
    val error = Formatter.throwable(throwable)
    val details = s"⚡$description" + EOL + Text.padLeft(error, 2)
    Text.colorizeCond(details, Console.RED, color)
  }

  def message(description: String, color: Boolean): String =
    Text.colorizeCond(Text.padLeft(description, 2), Console.RED, color)

  def success(
      description: String,
      color: Boolean
  ): String = {
    val details = s"✓ $description"
    Text.colorizeCond(details, Console.GREEN, color)
  }

  def throwable(throwable: Throwable): String = {
    val writer = new StringWriter
    throwable.printStackTrace(new PrintWriter(writer))
    writer.toString
  }
}
