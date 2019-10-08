package com.ayendo.testf.internal

import java.io.{PrintWriter, StringWriter}

import com.ayendo.testf.{Pure, Test}

object Formatter {
  def test(value: Test[Pure], color: Boolean = false): String =
    this.test(color, level = 0)(value)

  def test(
      color: Boolean,
      level: Int
  ): Test[Pure] => String = _.fold[Pure, String](
    effect = Formatter.test(color, level),
    error = message => error("error", Some(message), color),
    failure = throwable => failure("failure", throwable, color),
    group = { tests =>
      val messages = tests
        .map(Formatter.test(color, level + 1))
        .mkString(System.lineSeparator)
      Text.padLeft(messages, level * 2)
    },
    label = { (description, test) =>
      test.fold[Pure, String](
        effect = (test: Test[Pure]) => "???",
        error = message =>
          Text.padLeft(
            error(description, Some(message), color),
            columns = level * 2
          ),
        failure = throwable =>
          Text.padLeft(
            failure(description, throwable, color),
            columns = level * 2
          ),
        group = { tests =>
          val label =
            if (tests.forall(_.success)) success(description, color)
            else error(description, message = None, color)

          if (tests.isEmpty) label
          else {
            val details = tests.map(Formatter.test(color, level + 1))
            label + System.lineSeparator + details.mkString("\n")
          }
        },
        label = { (additional, test) =>
          Formatter.test(color, level)(
            Test.label(description + ", " + additional, test)
          )
        },
        message = { (additional, test) =>
          Formatter.test(color, level)(
            Test.label(description + ", " + additional, test)
          )
        },
        success = Text.padLeft(success(description, color), columns = level * 2)
      )
    },
    message = { (description, test) =>
      "???"
    },
    success = success("success", color)
  )

  def error(
      description: String,
      message: Option[String],
      color: Boolean
  ): String = {
    val value = s"✗ $description" + message
      .map(System.lineSeparator + Text.padLeft(_, 2))
      .getOrElse("")
    Text.colorizeCond(value, Console.RED, color)
  }

  def failure(
      description: String,
      throwable: Throwable,
      color: Boolean
  ): String = {
    val error = Formatter.throwable(throwable)
    val details = s"⚡$description" + System.lineSeparator + Text.padLeft(
      error,
      2
    )
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
