package com.ayendo.testf.laws

import cats.Semigroup
import com.ayendo.testf._
import com.ayendo.testf.scalacheck._
import org.typelevel.discipline.Laws

trait LawsAssertion {
  final def verify(ruleSet: Laws#RuleSet): Test[Pure] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => Test.label(id, Test.check(prop))
    }

    Semigroup[Test[Pure]]
      .combineAllOption(checks)
      .getOrElse(Test.success)
  }

  final def verify(description: String, ruleSet: Laws#RuleSet): Test[Pure] =
    Test.label(description, verify(ruleSet))
}

object LawsAssertion extends LawsAssertion
