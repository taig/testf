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

  val basic: Test[Pure] = Test.label(
    "basic",
    Test.of(
      Test.equal(
        Formatter.test(Fixture.error),
        "✗ error" + "\n  " + "foo"
      ) ~ "error",
      Test.equal(
        Formatter.test(Fixture.failure),
        "⚡failure" + "\n" + Fixture.stacktrace
      ) ~ "failure",
      Test.equal(
        Formatter.test(Fixture.success),
        "✓ success"
      ) ~ "success"
    )
  )

  val label: Test[Pure] = Test.label(
    "label",
    Test.of(
      Test.equal(
        Formatter.test(Fixture.label(Fixture.error)),
        "✗ foobar" + "\n  " + "foo"
      ) ~ "error",
      Test.equal(
        Formatter.test(Fixture.label(Fixture.failure)),
        "⚡foobar" + "\n" + Fixture.stacktrace
      ) ~ "failure",
      Test.equal(
        Formatter.test(Fixture.label(Fixture.success)),
        "✓ foobar"
      ) ~ "success"
    )
  )

  override def suite: IO[Test[Pure]] =
    Test.of(basic, label) ~ "FormatterTest" compile
}
