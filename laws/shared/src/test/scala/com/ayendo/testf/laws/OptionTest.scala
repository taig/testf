package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object OptionTest extends TestF {
  override val suite: Assert[IO] =
    Test.verify("MonadLaws", MonadTests[Option].monad[Int, Int, String]).liftIO
}
