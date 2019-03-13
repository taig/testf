package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._
import com.ayendo.testf.implicits._

object OptionTest extends TestF {
  override val suite: IO[Test[IO]] =
    IO.pure(
      "MonadLaws" @@ Test
        .verify(MonadTests[Option].monad[Int, Int, String])
        .mapK(Test.liftId))
}
