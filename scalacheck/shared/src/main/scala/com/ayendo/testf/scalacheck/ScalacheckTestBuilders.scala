package com.ayendo.testf.scalacheck

import cats.Id
import com.ayendo.testf._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders extends ScalacheckTestBuildersN {
  def check(description: String,
            prop: Prop,
            parameters: Parameters = Parameters.default): Test[Id, Unit] = {
    val result = org.scalacheck.Test.check(parameters, prop)

    if (result.passed) Test.success(description, ())
    else Test.error(description, Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckTestBuilders {
  private[scalacheck] def check(
      prop: (Test[Id, Unit] => Prop) => Prop,
      parameters: Parameters = Parameters.default): Test[Id, Unit] = {
    var report: Report = null

    val p: Test[Id, Unit] => Prop = { test =>
      report = test.compile
      Prop(report.success)
    }

    val result = org.scalacheck.Test.check(parameters, prop(p))

    if (result.passed) Test.result(report)
    else {
      val details = Pretty.pretty(result, Pretty.Params(2))
      Test.Result(Report.message(details, report))
    }
  }
}
