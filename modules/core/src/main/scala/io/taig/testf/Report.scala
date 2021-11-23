package io.taig.testf

final case class Report(name: String, evaluation: Evaluation[Report]):
  def summary: Result = evaluation match
    case Evaluation.Group(reports) => reports.map(_.summary).foldLeft(Result.Success)(_ && _)
    case Evaluation.Yield(result)  => result
