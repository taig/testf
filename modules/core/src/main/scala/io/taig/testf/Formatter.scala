package io.taig.testf

import io.taig.testf.Formatter.{Error, Failure, Skipped, Success}

object Formatter:
  def apply(report: Report, colors: Boolean): String =
    val builder = new StringBuilder
    apply(report, indent = "", builder, colors)
    builder.result()

  private val Success = "✓"
  private val Skipped = "☐"
  private val Error = "✗"
  private val Failure = "↯"
  private val Colon = ":"
  private val Linebreak = "\n"
  private val Space = " "

  private object Ansi {
    val Reset = "\u001B[0m"
    val Red = "\u001B[31m"
    val Green = "\u001B[32m"
    val Yellow = "\u001B[33m"
  }

  private def apply(report: Report, indent: String, builder: StringBuilder, colors: Boolean): Unit = report match
    case Report.Label(name, Report.Assertion(result)) =>
      colored(builder, color(result), colors) { builder =>
        builder
          .append(indent)
          .append(sign(result))
          .append(Space)
          .append(name)
          .append(Linebreak)
      }
    case Report.Label(name, report @ Report.Group(reports)) =>
      colored(builder, color(report.summary), colors) { builder =>
        builder
          .append(indent)
          .append(color(report.summary))
          .append(name)
          .append(Colon)
          .append(Ansi.Reset)
          .append(Linebreak)
      }

      reports.foreach(apply(_, indent + "  ", builder, colors))
    case Report.Label(outer, Report.Label(inner, report)) =>
      apply(Report.Label(s"$outer.$inner", report), indent, builder, colors)
    case Report.Assertion(_) =>
      builder.append(indent).append(Success).append("<unknown>").append(Linebreak)
    case Report.Group(reports) => reports.foreach(apply(_, indent, builder, colors))

  private def colored(builder: StringBuilder, color: String, enabled: Boolean)(
      f: StringBuilder => StringBuilder
  ): StringBuilder =
    if (enabled) f(builder.append(color)).append(Ansi.Reset) else f(builder)

  private val color: Result => String =
    case Result.Success    => Ansi.Green
    case Result.Skipped    => Ansi.Yellow
    case Result.Error(_)   => Ansi.Red
    case Result.Failure(_) => Ansi.Red

  private val sign: Result => String =
    case Result.Success    => Success
    case Result.Skipped    => Skipped
    case Result.Error(_)   => Error
    case Result.Failure(_) => Failure
