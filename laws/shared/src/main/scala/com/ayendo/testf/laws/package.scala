package com.ayendo.testf

import cats.Id
import cats.kernel.Semigroup
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

package object laws {
  implicit class TestLawsOps(val test: Test.type) extends AnyVal {
    def verify(name: String, ruleSet: Laws#RuleSet): Assert[Id] = {
      val checks = ruleSet.all.properties.map {
        case (id, prop) => Test.check(name + "." + id, prop)
      }

      Semigroup[Assert[Id]]
        .combineAllOption(checks)
        .fold[Assert[Id]](Test.success(name))(Test.label(name, _))
    }
  }
}
