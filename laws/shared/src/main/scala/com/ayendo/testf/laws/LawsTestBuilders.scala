package com.ayendo.testf.laws

import cats.Id
import cats.kernel.Semigroup
import com.ayendo.testf._
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

trait LawsTestBuilders {
  def verify(name: String, ruleSet: Laws#RuleSet): Test[Id, Assertion] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => check(name + "." + id, prop)
    }

    Semigroup[Test[Id, Assertion]]
      .combineAllOption(checks)
      .fold[Test[Id, Assertion]](success(name))(label(name, _))
  }
}
