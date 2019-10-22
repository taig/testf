package io.taig.testf

import cats.implicits._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckAssertions extends ScalacheckAssertionN {
  def check(
      prop: Prop,
      parameters: Parameters = Parameters.default
  ): Assertion[Pure] = {
    val result = org.scalacheck.Test.check(parameters, prop)
    if (result.passed) Test.unit
    else Test.error(Pretty.pretty(result, Pretty.Params(2)))
  }
}

object ScalacheckAssertions extends ScalacheckAssertions {
  private[testf] def checkTest(
      prop: (Assertion[Pure] => Prop) => Prop,
      parameters: Parameters = Parameters.default
  ): Assertion[Pure] = {
    var test: Assertion[Pure] = null

    val p: Assertion[Pure] => Prop = { x =>
      test = x
      Prop(Status.of(test) === Status.Success)
    }

    val result = org.scalacheck.Test.check(parameters, prop(p))
    if (result.passed) test else Test.message(Pretty.pretty(result), test)
  }
}
