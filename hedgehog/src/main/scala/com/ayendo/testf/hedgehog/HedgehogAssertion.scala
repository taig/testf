package com.ayendo.testf.hedgehog

import hedgehog._
import hedgehog.core._
import hedgehog.runner.{Test => HedgehogTest}
import com.ayendo.testf._

trait HedgehogAssertion {
  private val seed = Seed.fromTime()

  def check(
      property: Property,
      config: PropertyConfig = PropertyConfig.default
  ): Test[Pure] = {
    val report = Property.check(config, property, seed)
    val test = HedgehogTest("", property)

    report.status match {
      case OK => Test.success
      case Failed(_, _) | GaveUp =>
        val message = HedgehogTest.renderReport("", test, report, false)
        Test.error(message.substring(5))
    }
  }
}

object HedgehogAssertion extends HedgehogAssertion
