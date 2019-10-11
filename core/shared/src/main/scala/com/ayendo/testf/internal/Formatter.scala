package com.ayendo.testf.internal

import cats.implicits._
import com.ayendo.testf._

object Formatter {
  def test(value: Test[Pure], color: Boolean = false): String =
    Formatter.test(color, level = 0)(value).orEmpty

  def test(
      color: Boolean,
      level: Int
  ): Test[Pure] => Option[String] = {
    case Test.And(tests)       => both(color, level)(tests)
    case test: Test.Eval[Pure] => Formatter.test(color, level)(test.test)
    case Test.Error(message) =>
      error("unlabeled", message.some, color).some
    case Test.Failure(throwable) =>
      failure("unlabeled", throwable.some, color).some
    case Test.Label(description, test: Test.And[Pure]) =>
      group(color, level)(description, test)
    case Test.Label(description, test: Test.Or[Pure]) =>
      group(color, level)(description, test)
    case Test.Label(description, Test.Error(message)) =>
      error(description, message.some, color).some
    case Test.Label(description, Test.Failure(throwable)) =>
      failure(description, throwable.some, color).some
    case Test.Label(description, Test.Skip(_)) => skip(description, color).some
    case Test.Label(description, Test.Success) =>
      success(description, color).some
    case Test.Label(description, test)   => label(color, level)(description, test)
    case Test.Message(description, test) => "message".some
    case Test.Not(test)                  => "NOT".some
    case Test.Or(tests)                  => both(color, level)(tests)
    case Test.Skip(test)                 => skip(test.label.getOrElse(unlabeled), color).some
    case Test.Success                    => Options.when(level === 0)(success("unlabeled", color))
  }

  def group(
      color: Boolean,
      level: Int
  )(description: String, test: Test[Pure]): Option[String] = {
    val children = Formatter.children(test)
    val labels = children.mapFilter(_.label)
    val successes = children.filter(_.success)

    val descriptionWithCount =
      if (labels.length === children.length) description
      else if (successes === children) s"$description (${children.length})"
      else s"$description (${successes.length}/${children.length})"

    val errors = children
      .filter(_.label.isEmpty)
      .collect {
        case Test.Error(message) =>
          format(message, Options.when(color)(Console.RED))
        case Test.Failure(throwable) =>
          format(Text.print(throwable), Options.when(color)(Console.RED))
      }

    val members = children.mapFilter { test =>
      test.label *> Formatter.test(color, level)(test)
    }

    title(descriptionWithCount, Status.of(test), color).some |+|
      Options.when(errors.nonEmpty) {
        "\n" + Text.padLeft(errors.mkString("\n"), columns = 2)
      } |+|
      Options.when(members.nonEmpty) {
        "\n" + Text.padLeft(members.mkString("\n"), columns = 2)
      }
  }

  def title(description: String, status: Status, color: Boolean): String =
    status match {
      case Status.Error   => error(description, message = none, color)
      case Status.Failure => failure(description, throwable = none, color)
      case Status.Skip    => skip(description, color)
      case Status.Success => success(description, color)
    }

  def label(
      color: Boolean,
      level: Int
  )(description: String, test: Test[Pure]): Option[String] = {
    val title = Formatter.title(description, Status.of(test), color)

    val details = Formatter
      .test(color, level + 1)(test)
      .map("\n" + _)
      .map(Text.padLeft(_, columns = 2))

    title.some |+| details
  }

  val count: Test[Pure] => Int = {
    case Test.And(tests)       => tests.length
    case test: Test.Eval[Pure] => count(test.test)
    case Test.Error(_)         => 1
    case Test.Failure(_)       => 1
    case Test.Label(_, test)   => count(test)
    case Test.Message(_, test) => count(test)
    case Test.Not(test)        => count(test)
    case Test.Or(tests)        => tests.length
    case Test.Skip(_)          => 0
    case Test.Success          => 1
  }

  val children: Test[Pure] => List[Test[Pure]] = {
    case Test.And(tests)          => tests
    case test: Test.Eval[Pure]    => List(test)
    case test: Test.Error         => List(test)
    case test: Test.Failure       => List(test)
    case test: Test.Label[Pure]   => List(test)
    case test: Test.Message[Pure] => List(test)
    case test: Test.Not[Pure]     => List(test)
    case Test.Or(tests)           => tests
    case Test.Skip(_)             => List.empty
    case Test.Success             => List(Test.Success)
  }

  val unlabeled: String = "unlabeled"

  def both(color: Boolean, level: Int)(
      tests: List[Test[Pure]]
  ): Option[String] =
    tests.mapFilter(Formatter.test(color, level)) match {
      case Nil   => None
      case tests => tests.mkString("\n").some
    }

  def error(
      description: String,
      message: Option[String],
      color: Boolean
  ): String = {
    val error = message
      .map("\n" + Text.padLeft(_, 2))
      .getOrElse("")

    format(s"✗ $description" + error, Options.when(color)(Console.RED))
  }

  def failure(
      description: String,
      throwable: Option[Throwable],
      color: Boolean
  ): String = {
    val stacktrace = throwable
      .map(Text.print)
      .map(Text.padLeft(_, columns = 2))
      .map("\n" + _)
      .orEmpty

    format(s"⚡$description" + stacktrace, Options.when(color)(Console.RED))
  }

  def message(description: String, color: Boolean): String =
    format(description, Options.when(color)(Console.RED))

  def skip(description: String, color: Boolean): String =
    format(s"@ $description", Options.when(color)(Console.YELLOW))

  def success(description: String, color: Boolean): String =
    format(s"✓ $description", Options.when(color)(Console.GREEN))

  def format(text: String, color: Option[String]): String =
    color.fold(text)(Text.colorize(text, _))
}
