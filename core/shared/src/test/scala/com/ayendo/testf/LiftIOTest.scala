package com.ayendo.testf

import cats.{Eval, Id}
import cats.implicits._
import cats.effect.IO

object LiftIOTest extends TestF {
  val id: Assert[Id] = Test.pure[Id, Int]("id", 3).notEqual(0)

  val eval: Assert[Eval] =
    Test.defer("eval", Eval.later(1 + 1)).equalF(Eval.now(2))

  val io: Assert[IO] = Test.defer("io", IO(1 + 1)).notEqualF(IO.pure(0))

  override val suite: Assert[IO] = id.liftIO |+| eval.liftIO |+| io
}
