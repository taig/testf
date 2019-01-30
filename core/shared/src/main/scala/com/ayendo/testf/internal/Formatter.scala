package com.ayendo.testf.internal

import cats.implicits._
import com.ayendo.testf.Report

import scala.compat.Platform.EOL

object Formatter {
  val label: Report => String = {
    case Report.Error              => "error"
    case Report.Failure            => "failure"
    case Report.Label(_, label)    => label
    case Report.Message(report, _) => label(report)
    case Report.Group(reports) =>
      reports.map(label).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => tail.foldLeft(head)(_ + " |+| " + _)
      }
    case Report.Skip                  => "skip"
    case Report.Stacktrace(report, _) => label(report)
    case Report.Success               => "success"
  }

  def report(report: Report, color: Boolean): String =
    this.report(color, level = 0)(report)

  def report(color: Boolean, level: Int): Report => String = {
    case Report.Group(reports) if level === 0 =>
      reports.map(report(color, level + 1)).mkString(EOL)
    case group @ Report.Group(_) =>
      val label = this.label(group)

      if (group.success) success(label, color)
      else {
        val details = group.reports.map(this.report(color, level)).mkString(EOL)
        error(label, message = None, color) + EOL + Text.padLeft(details, 2)
      }
    case Report.Label(Report.Message(Report.Error, message), label) =>
      error(label, Some(message), color)
    case Report.Label(Report.Skip, label) => skip(label, color)
    case Report.Label(Report.Stacktrace(Report.Failure, throwable), label) =>
      failure(label, throwable, color)
    case Report.Label(Report.Label(report, l2), l1) =>
      this.report(color, level)(Report.Label(report, s"$l1, $l2"))
    case Report.Label(Report.Success, label) => success(label, color)
    case Report.Label(group @ Report.Group(_), label) =>
      if (group.success) success(label, color)
      else {
        val details = group.reports.map(this.report(color, level)).mkString(EOL)
        error(label, message = None, color) + EOL + Text.padLeft(details, 2)
      }
    case Report.Label(report, label) =>
      label + EOL + this.report(color, level)(report)
    case Report.Message(Report.Message(report, m2), m1) =>
      this.report(color, level)(Report.Message(report, m1 + EOL + m2))
    case Report.Message(Report.Success, message) => success(message, color)
    case Report.Message(Report.Label(Report.Message(report, m1), label), m2) =>
      this.report(color, level)(
        Report.Label(Report.Message(report, m1 + EOL + m2), label))
    case Report.Message(group @ Report.Group(_), message) =>
      val tint = if (group.success) Console.GREEN else Console.RED
      val details = Text.padLeft(Text.colorizeCond(message, tint, color), 2)
      report(color, level)(group) + EOL + details
    case Report.Message(label @ Report.Label(_, _), message) =>
      val tint = if (label.success) Console.GREEN else Console.RED
      val details = Text.padLeft(Text.colorizeCond(message, tint, color), 2)
      report(color, level)(label) + EOL + details
    case report => show"Unknown report format: $report"
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
