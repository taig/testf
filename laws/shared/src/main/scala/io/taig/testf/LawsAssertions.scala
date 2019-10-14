package io.taig.testf

import cats.Semigroup
import org.typelevel.discipline.Laws

trait LawsAssertions {
  final def verify(ruleSet: Laws#RuleSet): Assertion = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => Test.label(id, ScalacheckAssertions.check(prop))
    }

    Semigroup[Assertion]
      .combineAllOption(checks)
      .getOrElse(Test.unit)
  }

  final def verify(description: String, ruleSet: Laws#RuleSet): Assertion =
    Test.label(description, verify(ruleSet))
}

object LawsAssertions extends LawsAssertions
