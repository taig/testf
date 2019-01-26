package com.ayendo.testf

import cats.Functor
import cats.effect.IO
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class TestF {
  implicit def assertFToAssertIO[F[_]: Functor: LiftIO](
      assert: Assert[F]): Assert[IO] =
    assert.liftIO

  def suite: List[Assert[IO]]
}
