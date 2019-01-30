package com.ayendo.testf

import cats.{Eval, Id}
import cats.implicits._
import cats.effect.IO

object LiftIOTest extends TestF {
  val id: Test[Id, Unit] = Test.pure("id", 3).notEqual(0)

  val eval: Test[Eval, Unit] =
    Test.defer("eval", Eval.later(1 + 1)).equalF(Eval.now(2))

  val io: Test[IO, Unit] = Test.defer("io", IO(1 + 1)).notEqualF(IO.pure(0))

  override val suite: Test[IO, Unit] = id.liftIO |+| eval.liftIO |+| io
}
