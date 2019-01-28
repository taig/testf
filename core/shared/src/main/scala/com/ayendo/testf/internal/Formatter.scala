package com.ayendo.testf.internal

import cats.implicits._
import com.ayendo.testf.Report

import scala.compat.Platform.EOL

object Formatter {
  val description: Report => String = {
    case Report.Error(description, _)       => description
    case Report.Failure(description, _)     => description
    case Report.Group(_, Some(description)) => description
    case Report.Group(reports, None) =>
      reports.map(description).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => tail.foldLeft(head)(_ + " |+| " + _)
      }
    case Report.Skip(description)    => description
    case Report.Success(description) => description
  }

  def report(report: Report, color: Boolean): String =
    this.report(color, level = 0)(report)

  def report(color: Boolean, level: Int): Report => String = {
    case Report.Error(description, message) =>
      error(description, Some(message), color)
    case Report.Failure(description, throwable) =>
      failure(description, throwable, color)
    case Report.Group(reports, None) if level === 0 =>
      reports.map(report(color, level + 1)).mkString(EOL)
    case Report.Group(_, Some(description)) if level === 0 =>
      success(description, color)
    case group @ Report.Group(reports, description) =>
      val title = description.getOrElse(Formatter.description(group))

      if (group.isSuccess) success(title, color)
      else {
        val message = error(title, message = None, color)
        val details = reports.map(report(color, level + 1)).mkString(EOL)
        message + EOL + Text.padLeft(details, 2)
      }
    case Report.Skip(description)    => skip(description, color)
    case Report.Success(description) => success(description, color)
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
