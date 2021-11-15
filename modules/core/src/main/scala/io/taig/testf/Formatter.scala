package io.taig.testf

object Formatter:
  def apply(report: Report): String =
    val builder = new StringBuilder
    apply(report, indent = "", builder)
    builder.result()

  private val Success = "✓"
  private val Colon = ":"
  private val Linebreak = "\n"
  private val Space = " "

  private def apply(report: Report, indent: String, builder: StringBuilder): Unit = report match
    case Report.Label(name, Report.Assertion(_)) =>
      builder.append(indent).append(Success).append(Space).append(name).append(Linebreak)
    case Report.Label(name, Report.Group(reports)) =>
      builder.append(indent).append(name).append(Colon).append(Linebreak)
      reports.foreach(apply(_, indent + "  ", builder))
    case Report.Label(outer, Report.Label(inner, report)) =>
      apply(Report.Label(s"$outer.$inner", report), indent, builder)
    case Report.Assertion(_) =>
      builder.append(indent).append(Success).append("<unknown>").append(Linebreak)
    case Report.Group(reports) => reports.foreach(apply(_, indent, builder))
