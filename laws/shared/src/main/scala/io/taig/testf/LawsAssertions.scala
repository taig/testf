package io.taig.testf

import cats.Semigroup
import org.typelevel.discipline.Laws

trait LawsAssertions {
  final def verify(ruleSet: Laws#RuleSet): Assertion[Pure] = {
    val checks = ruleSet.all.properties.map {
      case (id, prop) => Test.label(id, ScalacheckAssertions.check(prop))
    }

    Semigroup[Assertion[Pure]]
      .combineAllOption(checks)
      .getOrElse(Test.unit)
  }

  final def verify(
      description: String,
      ruleSet: Laws#RuleSet
  ): Assertion[Pure] =
    Test.label(description, verify(ruleSet))
}

object LawsAssertions extends LawsAssertions
