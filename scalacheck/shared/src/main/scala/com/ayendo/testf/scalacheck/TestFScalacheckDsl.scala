package com.ayendo.testf.scalacheck

import cats.effect.IO
import com.ayendo.testf.{Result, Test}
import org.scalacheck.Test.Parameters
import org.scalacheck.util.Pretty
import org.scalacheck.{Prop, Test => SCTest}
import sourcecode.Name

trait TestFScalacheckDsl {
  def check(description: String, prop: Prop, parameters: Parameters): Test = {
    val result = SCTest.check(parameters, prop)

    if (result.passed) Test.Assert(description, IO.pure(Result.Success))
    else Test.Assert(description, IO.pure(Result.Error(Pretty.pretty(result))))
  }

  def check(description: String, prop: Prop): Test =
    check(description, prop, Parameters.default)

  def check(prop: Prop, parameters: Parameters)(implicit name: Name): Test =
    check(name.value, prop, parameters)

  def check(prop: Prop)(implicit name: Name): Test =
    check(prop, Parameters.default)
}
