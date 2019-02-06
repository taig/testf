package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object OptionTest extends TestF {
  override val suite: IO[Test[Unit]] =
    IO.pure(
      Test.verify("MonadLaws", MonadTests[Option].monad[Int, Int, String]))
}
