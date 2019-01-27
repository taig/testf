package com.ayendo.testf.scalacheck

import cats.Id
import com.ayendo.testf._
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

trait ScalacheckTestBuilders {
  def check(description: String,
            prop: Prop,
            parameters: Parameters = Parameters.default): Assert[Id] = {
    val result = org.scalacheck.Test.check(parameters, prop)

    if (result.passed) success(description)
    else error(description, Pretty.pretty(result, Pretty.Params(2)))
  }
}
