package io.taig.testf

import cats.effect.IO
import cats.implicits._
import io.taig.testf.internal.{Formatter, Text}
import io.taig.testf.dsl._

object FormatterTest extends TestApp {
  object Fixture {
    val exception: Exception = new Exception("exception")

    val stacktrace: String =
      Text.padLeft(Text.print(Fixture.exception), columns = 2)

    val error: Assertion[Pure] = Test.error("foo")

    val failure: Assertion[Pure] = Test.failure(exception)

    def label(test: Assertion[Pure]): Assertion[Pure] =
      Test.label("foobar", test)

    val skip: Assertion[Pure] = Test.skip(error)

    val success: Assertion[Pure] = Test.unit
  }

  val basic: Assertion[Pure] = test("basic")(
    test("error")(
      test("root") {
        equal("✗ unlabeled" + "\n  " + "foo")(Formatter.test(Fixture.error))
      },
      test("labeled") {
        equal("✗ foobar" + "\n  " + "foo")(
          Formatter.test(Fixture.label(Fixture.error))
        )
      }
    ),
    test("failure")(
      test("root") {
        equal("⚡unlabeled" + "\n" + Fixture.stacktrace)(
          Formatter.test(Fixture.failure)
        )
      },
      test("labeled") {
        equal("⚡foobar" + "\n" + Fixture.stacktrace)(
          Formatter.test(Fixture.label(Fixture.failure))
        )
      }
    ),
    test("success")(
      test("root") {
        equal("✓ unlabeled")(Formatter.test(Fixture.success))
      },
      test("labeled") {
        equal("✓ foobar")(Formatter.test(Fixture.label(Fixture.success)))
      }
    ),
    test("skip")(
      test("root") {
        equal("@ unlabeled")(Formatter.test(Fixture.skip))
      },
      test("labeled") {
        equal("@ foobar")(Formatter.test(Fixture.label(Fixture.skip)))
      }
    ),
    test("skip dummy")(Fixture.skip)
  )

  val skip: Assertion[Pure] = test("skip")(
    test("falls back to inner label") {
      val test = Test.skip(Fixture.label(Test.empty))
      equal("@ foobar")(Formatter.test(test))
    }
  )

  val unlabeled: Assertion[Pure] = test("unlabeled")(
    test("shows amount of unlabeled tests") {
      val test = Fixture.label(Test.allOf(Fixture.success, Fixture.success))
      equal("✓ foobar (2)")(Formatter.test(test))
    },
    test("shows amount succeeded tests") {
      val test = Fixture.label(Test.allOf(Fixture.success, Fixture.error))
      equal("✗ foobar (1/2)" + "\n  " + "foo")(Formatter.test(test))
    }
  )

  override def suite: IO[Assertion[Pure]] =
    test("FormatterTest")(basic, skip, unlabeled).compile
}
