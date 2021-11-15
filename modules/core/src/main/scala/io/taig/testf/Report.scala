package io.taig.testf

enum Report:
  case Assertion(result: Result)
  case Label(name: String, report: Report)
  case Group(reports: List[Report])

  def isSuccess: Boolean = this match
    case Assertion(result) => result.isSuccess
    case Label(_, report)  => report.isSuccess
    case Group(reports)    => reports.forall(_.isSuccess)

  def failure: Option[Throwable] = this match
    case Assertion(result) => result.failure
    case Label(_, report)  => report.failure
    case Group(reports)    =>
      reports.foldLeft[Option[Throwable]](None) {
        case (None, report) => report.failure
        case (throwable @ Some(_), _) => throwable
      }
