package io.taig.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.laws.discipline.MonadTests
import io.taig.testf._
import io.taig.testf.dsl._

object OptionTest extends TestF {
  val monadLaws: Assertion =
    verify("MonadLaws", MonadTests[Option].monad[Int, Int, String])

  override val suite: IO[Assertion] = test("OptionTest")(monadLaws).compile
}
