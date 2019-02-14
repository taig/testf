package com.ayendo.testf.laws

import cats.Id
import cats.kernel.Semigroup
import com.ayendo.testf._
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

trait LawsTestBuilders {
  def verify(name: String, ruleSet: Laws#RuleSet): Test[Id, Unit] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => Test.check(name + "." + id, prop)
    }

    Semigroup[Test[Id, Unit]]
      .combineAllOption(checks)
      .fold[Test[Id, Unit]](Test.unit(name))(Test.label(name, _))
  }
}
