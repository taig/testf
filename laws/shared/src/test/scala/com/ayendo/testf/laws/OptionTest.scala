package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._
import com.ayendo.testf.dsl._

object OptionTest extends TestF {
  val monadLaws: Test[Pure] =
    Test.verify("MonadLaws", MonadTests[Option].monad[Int, Int, String])

  override val suite: IO[Test[Pure]] = test("OptionTest")(monadLaws).compile
}
