package com.ayendo.testf.scalacheck

import cats.effect.IO
import com.ayendo.testf.{Result, Test}
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty
import org.scalacheck.{Prop, Test => SCTest}

trait TestFScalacheckDsl {
  def check(description: String,
            prop: Prop,
            parameters: Parameters = Parameters.default): Test = {
    val result = SCTest.check(parameters, prop)

    if (result.passed) Test.Assert(description, IO.pure(Result.Success))
    else Test.Assert(description, IO.pure(Result.Error(Pretty.pretty(result))))
  }
}
