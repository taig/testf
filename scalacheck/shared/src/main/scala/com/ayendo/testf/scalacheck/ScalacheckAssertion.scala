package com.ayendo.testf.scalacheck

import com.ayendo.testf._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckAssertion extends ScalacheckAssertionN {
  def check(
      prop: Prop,
      parameters: Parameters = Parameters.default
  ): Test[Pure] = {
    val result = org.scalacheck.Test.check(parameters, prop)
    if (result.passed) Test.success
    else Test.error(Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckAssertion extends ScalacheckAssertion {
  private[scalacheck] def checkTest(
      prop: (Test[Pure] => Prop) => Prop,
      parameters: Parameters = Parameters.default
  ): Test[Pure] = {
    var test: Test[Pure] = null

    val p: Test[Pure] => Prop = { x =>
      test = x
      Prop(test.success)
    }

    val result = org.scalacheck.Test.check(parameters, prop(p))
    if (result.passed) test else Test.message(Pretty.pretty(result))(test)
  }
}
