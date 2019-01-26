package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._

object OptionTest extends TestF {
  override val suite: List[Assert[IO]] = List(
    Test.verify("MonadLaws", MonadTests[Option].monad[Int, Int, String])
  )
}
