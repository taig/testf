package com.ayendo.testf

import cats.implicits._
import cats.effect.IO
import com.ayendo.testf.internal.{Formatter, Text}

object FormatterTest extends TestF {
  val exception: Exception = new Exception("exception")

  val error: Test[Pure] = Test.error("foo")

  val failure: Test[Pure] = Test.failure(exception)

  def label(test: Test[Pure]): Test[Pure] = Test.label("foobar", test)

  val success: Test[Pure] = Test.success

  val formatError: Test[Pure] =
    Test.equal(
      Formatter.test(error, color = false),
      "✗ error" + "\n  " + "foo"
    ) ~ "error"

  val formatFailure: Test[Pure] = {
    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)
    Test.equal(
      Formatter.test(failure, color = false),
      "⚡failure" + "\n" + details
    ) ~ "failure"
  }

  val formatSuccess: Test[Pure] =
    Test.equal(
      Formatter.test(success, color = false),
      "✓ success"
    ) ~ "success"

  val formatLabelError: Test[Pure] =
    Test.equal(
      Formatter.test(label(error), color = false),
      "✗ foobar" + "\n  " + "foo"
    ) ~ "labelError"

  val formatLabelFailure: Test[Pure] = {
    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    Test.equal(
      Formatter.test(label(failure), color = false),
      "⚡foobar" + "\n" + details
    ) ~ "labelFailure"
  }

  val formatLabelSuccess: Test[Pure] =
    Test.equal(
      Formatter.test(label(success), color = false),
      "✓ foobar"
    ) ~ "labelSuccess"

  val formatLabelAnonymousGroup: Test[Pure] =
    Test.equal(
      Formatter.test(label(Test.of(Test.success, Test.success)), color = false),
      "✓ foobar"
    ) ~ "labelAnonymousGroup"

  override val suite: IO[Test[Pure]] =
    Test.of(
      formatError,
      formatFailure,
      formatSuccess,
      formatLabelError,
      formatLabelFailure,
      formatLabelSuccess,
      formatLabelAnonymousGroup
    ) ~ "FormatterTest" compile
}
