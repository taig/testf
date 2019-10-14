package io.taig.testf.hedgehog

import hedgehog._
import hedgehog.core._
import hedgehog.runner.{Test => HedgehogTest}
import io.taig.testf._

trait HedgehogAssertions {
  private val seed = Seed.fromTime()

  def check(
      property: Property,
      config: PropertyConfig = PropertyConfig.default
  ): Assertion = {
    val report = Property.check(config, property, seed)
    val test = HedgehogTest("", property)

    report.status match {
      case OK => Test.unit
      case Failed(_, _) | GaveUp =>
        val message = HedgehogTest.renderReport("", test, report, false)
        Test.error(message.substring(5))
    }
  }
}

object HedgehogAssertions extends HedgehogAssertions
