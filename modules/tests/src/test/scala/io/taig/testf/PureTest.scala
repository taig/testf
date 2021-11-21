package io.taig.testf

import cats.Id as Identity
import io.taig.testf.dsl.*

object PureTest extends PureTestApp:
  override val suite: Test[Pure] = group("Pure calculations")(
    test("1 + 1") {
      Assertion.equals(obtained = 1 + 1, expected = 2)
    },
    test("10²") {
      Assertion.equals(obtained = math.pow(10, 2), expected = 100)
    }
  )
