package com.ayendo.testf

import cats.effect.IO
import cats.implicits._

object AdditionTest extends TestF {
  val onePlusOne: Assert = Test.pure("onePlusOne", 1 + 1).equal(2)

  val zeroPlusZero: Assert = Test("zeroPlusZero", 0 + 0).equalF(IO(0))

  override def suite: List[Assert] = List(onePlusOne, zeroPlusZero)
}
