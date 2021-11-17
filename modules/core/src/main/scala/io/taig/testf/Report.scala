package io.taig.testf

enum Report:
  case Assertion(result: Result)
  case Label(name: String, report: Report)
  case Group(reports: List[Report])

  def summary: Result = this match
    case Assertion(result) => result
    case Label(_, report)  => report.summary
    case Group(reports) =>
      reports.map(_.summary).foldLeft(Result.Success) {
        case (Result.Success, Result.Success)                                 => Result.Success
        case (Result.Success, result @ (Result.Error(_) | Result.Failure(_))) => result
        case (result @ (Result.Error(_) | Result.Failure(_)), Result.Success | Result.Skipped | Result.Error(_)) =>
          result
        case (Result.Error(_), result @ Result.Failure(_))                     => result
        case (result @ Result.Failure(_), Result.Failure(_) | Result.Error(_)) => result
        case (Result.Skipped, result)                                          => result
        case (result, Result.Skipped)                                          => result
      }
