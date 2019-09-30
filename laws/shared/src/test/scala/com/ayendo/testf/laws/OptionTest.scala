package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object OptionTest extends TestF {
  val monadLaws =
    Test.verify("MonadLaws", MonadTests[Option].monad[Int, Int, String])

  override val suite: IO[Test[Pure]] = (monadLaws ~ "OptionTest").compile
}
