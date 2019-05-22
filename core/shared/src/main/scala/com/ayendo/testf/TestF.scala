package com.ayendo.testf

import cats.effect.IO
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class TestF {
  def suite: IO[Test[Pure]]
}
