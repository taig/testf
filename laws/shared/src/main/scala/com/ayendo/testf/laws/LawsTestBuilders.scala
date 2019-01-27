package com.ayendo.testf.laws

import cats.Id
import cats.kernel.Semigroup
import com.ayendo.testf._
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

trait LawsTestBuilders {
  def verify(name: String, ruleSet: Laws#RuleSet): Assert[Id] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => check(name + "." + id, prop)
    }

    Semigroup[Assert[Id]]
      .combineAllOption(checks)
      .fold[Assert[Id]](success(name))(label(name, _))
  }
}
