package com.ayendo.testf

import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty

package object scalacheck {
  implicit class ScalacheckTestOps(val test: Test.type) extends AnyVal {
    def check(description: String,
              prop: Prop,
              parameters: Parameters = Parameters.default): Assert = {
      val result = org.scalacheck.Test.check(parameters, prop)

      if (result.passed) Test.success(description)
      else Test.error(description, Pretty.pretty(result))
    }
  }
}
