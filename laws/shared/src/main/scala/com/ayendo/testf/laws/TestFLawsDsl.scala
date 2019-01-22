package com.ayendo.testf.laws

import cats.Id
import cats.kernel.Semigroup
import com.ayendo.testf.scalacheck.check
import com.ayendo.testf.{Test, label, succeed}
import org.typelevel.discipline.Laws

trait TestFLawsDsl {
  def verify(name: String, ruleSet: Laws#RuleSet): Test[Id] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => check(name + "." + id, prop)
    }

    Semigroup[Test[Id]]
      .combineAllOption(checks)
      .fold[Test[Id]](succeed(name))(label(name, _))
  }
}
