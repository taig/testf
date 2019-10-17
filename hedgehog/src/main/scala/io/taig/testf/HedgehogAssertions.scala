package io.taig.testf

import hedgehog._
import hedgehog.core._
import hedgehog.runner.{Test => HedgehogTest}

trait HedgehogAssertions {
  private val seed = Seed.fromTime()

  def check(
      property: Property,
      config: PropertyConfig = PropertyConfig.default
  ): Assertion[Pure] = {
    val report = Property.check(config, property, seed)
    val test = HedgehogTest("", property)

    report.status match {
      case OK => Test.unit
      case Failed(_, _) | GaveUp =>
        val message = HedgehogTest.renderReport(
          className = "",
          test,
          report,
          ansiCodesSupported = false
        )
        Test.error(message.substring(5))
    }
  }
}

object HedgehogAssertions extends HedgehogAssertions
