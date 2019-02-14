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
    var test: Test[Id, Unit] = null

    val p: Test[Id, Unit] => Prop = { x =>
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
