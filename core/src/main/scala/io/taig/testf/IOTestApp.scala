package io.taig.testf

import cats.effect.IO
import org.portablescala.reflect.annotation.EnableReflectiveInstantiation

@EnableReflectiveInstantiation
abstract class IOTestApp {
  def suite: IO[Assertion[Pure]]
}
