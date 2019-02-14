package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object OptionTest extends TestF {
  override val suite: Test[IO, Unit] =
    Test
      .verify("MonadLaws", MonadTests[Option].monad[Int, Int, String])
      .mapK(Test.liftId)
}
