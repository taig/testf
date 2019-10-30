package io.taig.testf

import cats.implicits._

trait AbstractAutoTestDiscovery {
  protected type F[α]

  protected def auto: Assertion[F] = Test.empty

  def additional: Assertion[F] = Test.empty

  final def all: Assertion[F] = {
    val name = getClass.getName.replace("$", "")
    Test.test(name)(auto |+| additional)
  }
}

trait AutoTestDiscovery[G[_]] extends AbstractAutoTestDiscovery {
  override final type F[α] = G[α]
}
