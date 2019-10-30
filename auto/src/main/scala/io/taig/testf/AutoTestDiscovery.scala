package io.taig.testf

import cats.implicits._

trait AbstractAutoTestDiscovery {
  protected type __F[α]

  protected def auto: Assertion[__F] = Test.empty

  def additional: Assertion[__F] = Test.empty

  final def all: Assertion[__F] = {
    val name = getClass.getName.replace("$", "")
    Test.test(name)(auto |+| additional)
  }
}

trait AutoTestDiscovery[G[_]] extends AbstractAutoTestDiscovery {
  override final type __F[α] = G[α]
}
