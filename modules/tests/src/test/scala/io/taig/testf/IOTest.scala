package io.taig.testf

import cats.effect.IO
import io.taig.testf.dsl.*

object IOTest extends IOTestApp:
  override val suite: IO[Test[IO]] = IO {
    group.parallel("fuck you")(
      testF("1 + 1") {
        IO(Assertion.equals(obtained = 1 + 1, expected = 2))
      }
    )
  }
