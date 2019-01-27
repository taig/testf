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
      summaries.map(description).distinct match {
        case Nil          => ""
        case head :: Nil  => head
        case head :: tail => tail.foldLeft(head)(_ + " |+| " + _)
      }
    case Summary.Skip(description)    => description
    case Summary.Success(description) => description
  }

  def summary(summary: Summary, color: Boolean): String =
    this.summary(color, level = 0)(summary)

  def summary(color: Boolean, level: Int): Summary => String = {
    case Summary.Error(description, message) =>
      error(description, Some(message), color)
    case Summary.Failure(description, throwable) =>
      failure(description, throwable, color)
    case Summary.Group(summaries, None) if level === 0 =>
      summaries.map(summary(color, level + 1)).mkString(EOL)
    case Summary.Group(_, Some(description)) if level === 0 =>
      success(description, color)
    case group @ Summary.Group(summaries, description) =>
      val title = description.getOrElse(Formatter.description(group))

      if (group.isSuccess) success(title, color)
      else {
        val message = error(title, message = None, color)
        val details = summaries.map(summary(color, level + 1)).mkString(EOL)
        message + EOL + Text.padLeft(details, 2)
      }
    case Summary.Skip(description)    => skip(description, color)
    case Summary.Success(description) => success(description, color)
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
