package com.ayendo.testf.scalacheck

import com.ayendo.testf._
import com.ayendo.testf.implicits._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders extends ScalacheckTestBuildersN {
  def check(description: String,
            prop: Prop,
            parameters: Parameters = Parameters.default): Test = {
    val result = org.scalacheck.Test.check(parameters, prop)

    if (result.passed) Test.success(description)
    else description @@ Test.error(Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckTestBuilders {
  private[scalacheck] def check(
      prop: (Test => Prop) => Prop,
      parameters: Parameters = Parameters.default): Test = {
    var test: Test = null

    val p: Test => Prop = { x =>
      test = x
      Prop(test.success)
    }

    val result = org.scalacheck.Test.check(parameters, prop(p))

    if (result.passed) test
    else {
      val details = Pretty.pretty(result, Pretty.Params(2))
      Test.Message(details, test)
    }
  }
}
