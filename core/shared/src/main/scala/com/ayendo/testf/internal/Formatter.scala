package com.ayendo.testf.internal

import cats.implicits._
import com.ayendo.testf.Summary

import scala.compat.Platform.EOL

object Formatter {
  val description: Summary => String = {
    case Summary.Error(description, _)       => description
    case Summary.Failure(description, _)     => description
    case Summary.Group(_, Some(description)) => description
    case Summary.Group(summaries, None) =>
      summaries.map(description).distinct.foldLeft("")(_ + " |+| " + _)
    case Summary.Skip(description)    => description
    case Summary.Success(description) => description
  }

  def summary(summary: Summary, color: Boolean): String =
    this.summary(color, expanded = summary.isErrorOrFailure, level = 0)(summary)

  def summary(color: Boolean,
              expanded: Boolean,
              level: Int): Summary => String = {
    case Summary.Error(description, message) =>
      error(description, Some(message), color)
    case Summary.Failure(description, throwable) =>
      failure(description, throwable, color)
    case Summary.Success(description) => success(description, color)
    case group: Summary.Group if level === 0 =>
      val description = this.description(group)

      val title =
        if (group.isSuccess) success(description, color)
        else error(description, message = None, color)

      if (expanded) {
        val details =
          group.summaries.map(summary(color, expanded, level + 1)).mkString(EOL)
        Text.padLeft(title + EOL + details, level * 2)
      } else title
//    case group @ Summary.Group(_, _: Summary.Row, Some(description)) =>
//      if (group.isSuccess) success(description, color)
//      else error(description, message = None, color)
//    case Summary.Group(left, right: Summary.Row, None) =>
//      summary(color, expanded, level)(left) + EOL +
//        summary(color, expanded, level)(right)
    case _: Summary.Group => ???
  }

  def error(description: String,
            message: Option[String],
            color: Boolean): String = {
    val value = s"✗ $description" +
      message.map(EOL + Text.padLeft(_, 2)).orEmpty
    if (color) Text.colorize(value, Console.RED) else value
  }

  def failure(description: String,
              throwable: Throwable,
              color: Boolean): String = {
    val error = Formatter.throwable(throwable)
    val value = s"⚡$description" + EOL + Text.padLeft(error, 2)

    if (color) Text.colorize(value, Console.RED) else value
  }

  def success(description: String, color: Boolean): String = {
    val value = s"✓ $description"
    if (color) Text.colorize(value, Console.GREEN) else value
  }

  def throwable(throwable: Throwable): String =
    throwable.getMessage +
      EOL +
      throwable.getStackTrace.mkString("", EOL, EOL)
}
