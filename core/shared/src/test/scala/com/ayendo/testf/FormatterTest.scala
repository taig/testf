package com.ayendo.testf

import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}

object FormatterTest extends TestF {
  object Fixture {
    val exception: Exception = new Exception("exception")

    val stacktrace: String =
      Text.padLeft(Formatter.throwable(Fixture.exception), columns = 2)

    val error: Test[Pure] = Test.error("foo")

    val failure: Test[Pure] = Test.failure(exception)

    def label(test: Test[Pure]): Test[Pure] = Test.label("foobar", test)

    val success: Test[Pure] = Test.success
  }

  val basic: Test[Pure] = Test("basic")(
    Test("error") {
      Test.equal(
        Formatter.test(Fixture.error),
        "✗ error" + "\n  " + "foo"
      )
    },
    Test("failure") {
      Test.equal(
        Formatter.test(Fixture.failure),
        "⚡failure" + "\n" + Fixture.stacktrace
      )
    },
    Test("success") {
      Test.equal(
        Formatter.test(Fixture.success),
        "✓ success"
      )
    }
  )

  val label: Test[Pure] = Test("label")(
    Test("error") {
      Test.equal(
        Formatter.test(Fixture.label(Fixture.error)),
        "✗ foobar" + "\n  " + "foo"
      )
    },
    Test("failure") {
      Test.equal(
        Formatter.test(Fixture.label(Fixture.failure)),
        "⚡foobar" + "\n" + Fixture.stacktrace
      )
    },
    Test("success") {
      Test.equal(
        Formatter.test(Fixture.label(Fixture.success)),
        "✓ foobar"
      )
    }
  )

  override def suite: IO[Test[Pure]] =
    Test("FormatterTest")(basic, label).compile
}
