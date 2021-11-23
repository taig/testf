package io.taig.testf

import io.taig.testf.Formatter.{Error, Failure, Skipped, Success}

import java.io.{PrintWriter, StringWriter}

object Formatter:
  private val Success = "✓"
  private val Skipped = "☐"
  private val Error = "✗"
  private val Failure = "↯"
  private val Colon = ":"
  private val Linebreak = "\n"
  private val Space = " "

  opaque type Ansi = String

  object Ansi:
    val Reset: Ansi = Console.RESET
    val Red: Ansi = Console.RED
    val Green: Ansi = Console.GREEN
    val Yellow: Ansi = Console.YELLOW

  extension (ansi: Ansi) def value: String = ansi

  def apply(report: Report, colors: Boolean): String = ??? // Formatter(report, indent = "", colors)

//  private def apply(report: Evaluation, indent: String, colors: Boolean): String = report match
//    case Evaluation.Label(name, Evaluation.Assertion(result)) =>
//      maybeColorizeLines(indent + sign(result) + Space + name, color(result), colors) +
//        result.failure
//          .map { throwable =>
//            maybeColorizeLines(Colon, Ansi.Red, colors) + Linebreak +
//              padLeft(maybeColorizeLines(Formatter.stacktrace(throwable), Ansi.Red, colors), indent + "  ")
//          }
//          .getOrElse("")
//    case Evaluation.Label(name, report @ Evaluation.Group(reports)) =>
//      maybeColorizeLines(indent + name + Colon, color(report.summary), colors) +
//        reports.map(report => Formatter(report, indent + "  ", colors)).mkString(Linebreak, Linebreak, "")
//    case Evaluation.Label(outer, Evaluation.Label(inner, report)) =>
//      apply(Evaluation.Label(s"$outer.$inner", report), indent, colors)
//    case Evaluation.Assertion(_)   => indent + Success + "<unknown>"
//    case Evaluation.Group(reports) => reports.map(Formatter(_, indent, colors)).mkString
//
//  private def maybeColorizeLines(text: String, color: Ansi, enabled: Boolean): String =
//    if (enabled) colorizeLines(text, color) else text
//
//  private def colorizeLines(text: String, color: Ansi): String =
//    text
//      .split('\n')
//      .map(colorize(_, color))
//      .mkString("\n")
//
//  private def colorize(text: String, color: Ansi): String =
//    color.value + text + Ansi.Reset.value
//
//  private def stacktrace(throwable: Throwable): String =
//    val writer = new StringWriter
//    throwable.printStackTrace(new PrintWriter(writer))
//    writer.toString
//
//  private def padLeft(value: String, indent: String): String =
//    if (indent == "") value
//    else
//      value
//        .split("\n")
//        .map(value => indent + value)
//        .mkString("\n")
//
//  private val color: Result => Ansi =
//    case Result.Success    => Ansi.Green
//    case Result.Skipped    => Ansi.Yellow
//    case Result.Error(_)   => Ansi.Red
//    case Result.Failure(_) => Ansi.Red
//
//  private val sign: Result => String =
//    case Result.Success    => Success
//    case Result.Skipped    => Skipped
//    case Result.Error(_)   => Error
//    case Result.Failure(_) => Failure
