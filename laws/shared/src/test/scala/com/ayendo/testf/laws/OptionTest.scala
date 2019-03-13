package com.ayendo.testf.laws

import cats.Id
import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import com.ayendo.testf._
import com.ayendo.testf.implicits._

object OptionTest extends TestF {
  val monadLaws: Test[Id] = "MonadLaws" @@ Test.verify(
    MonadTests[Option].monad[Int, Int, String])

  override val suite: IO[List[Test.Result]] =
    IO.pure(List(monadLaws).map(_.compile))
}
