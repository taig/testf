package com.ayendo.testf.laws

import cats.kernel.Semigroup
import com.ayendo.testf.scalacheck.check
import com.ayendo.testf.{Test, label, succeed}
import org.typelevel.discipline.Laws

trait TestFLawsDsl {
  def verify(name: String, ruleSet: Laws#RuleSet): Test = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => check(name + "." + id, prop)
    }

    Semigroup[Test]
      .combineAllOption(checks)
      .fold[Test](succeed(name))(label(name, _))
  }
}
