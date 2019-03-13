package com.ayendo.testf.scalacheck

import cats.Id
import com.ayendo.testf._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders extends ScalacheckTestBuildersN {
  def check(prop: Prop,
            parameters: Parameters = Parameters.default): Test[Id] = {
    val result = org.scalacheck.Test.check(parameters, prop)
    if (result.passed) Test.success
    else Test.error(Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckTestBuilders extends ScalacheckTestBuilders {
  private[scalacheck] def checkTest(
      prop: (Test[Id] => Prop) => Prop,
      parameters: Parameters = Parameters.default): Test[Id] = {
    var test: Test[Id] = null

    val p: Test[Id] => Prop = { x =>
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
