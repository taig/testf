package com.ayendo.testf.scalacheck

import com.ayendo.testf._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders extends ScalacheckTestBuildersN {
  def check(prop: Prop,
            parameters: Parameters = Parameters.default): Test[Nothing] = {
    val result = org.scalacheck.Test.check(parameters, prop)
    if (result.passed) Test.success
    else Test.error(Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckTestBuilders extends ScalacheckTestBuilders {
  private[scalacheck] def checkTest(
      prop: (Test[Nothing] => Prop) => Prop,
      parameters: Parameters = Parameters.default): Test[Nothing] = {
    var test: Test[Nothing] = null

    val p: Test[Nothing] => Prop = { x =>
      test = x
      Prop(test.success)
    }

    val result = org.scalacheck.Test.check(parameters, prop(p))

    if (result.passed) test
    else {
      val details = Pretty.pretty(result, Pretty.Params(2))
      Test.Message[Nothing](details, test)
    }
  }
}
