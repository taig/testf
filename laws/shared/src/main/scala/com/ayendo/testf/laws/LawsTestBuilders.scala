package com.ayendo.testf.laws

import cats.kernel.Semigroup
import com.ayendo.testf._
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

trait LawsTestBuilders {
  def verify(name: String, ruleSet: Laws#RuleSet): Test = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => Test.check(name + "." + id, prop)
    }

    Semigroup[Test]
      .combineAllOption(checks)
      .fold[Test](Test.success(name))(Test.label(name, _))
  }
}
