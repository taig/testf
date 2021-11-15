package io.taig.testf

import cats.Id as Identity
import io.taig.testf.dsl.*

object PureTest extends PureTestApp:
  override val suite: Test[Pure] = test("1 + 1") {
    Assertion.equals(obtained = 1 + 1, expected = 3)
  }
