package com.ayendo.testf

import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import com.ayendo.testf.implicits._
import sourcecode.Name

object TestTest extends TestF {
  def showTest(test: Test, expected: String)(implicit name: Name): Test =
    name.value @@ Test.equal(Formatter.test(test, duration = 0, color = false),
                             expected)

  val showError: Test =
    showTest("error" @@ Test.error("reason"),
             """✗ error (0ms)
               |  reason""".stripMargin)

  val showFailure: Test = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest("failure" @@ Test.failure(exception),
             s"""⚡failure (0ms)
                |$details""".stripMargin)
  }

  val showSkip: Test =
    showTest(Test.skip(Test.success), "~ skip")

  val showSuccess: Test =
    showTest(Test.success("success"), "✓ success (0ms)")

  val showGroupRootWithLabelSuccess: Test = {
    val test = Test.label(
      "success",
      Test.success("s1") |+| Test.success("s2"),
    )

    showTest(test, "✓ success (0ms)")
  }

  val showGroupNestedWithoutLabelSuccess: Test = {
    val test = Test.group(Test.success("s1") |+| Test.success("s2"))

    showTest(test, "✓ s1 |+| s2 (0ms)")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test = {
    val test = Test.group(Test.success("s1") |+| Test.success("s1"))

    showTest(test, "✓ s1 (0ms)")
  }

  val showGroupNestedWithLabelSuccess: Test = {
    val test = Test.group(
      Test.label("group",
                 Test.group(Test.success("s1") |+| Test.success("s2"))))

    showTest(test, "✓ group (0ms)")
  }

  override val suite: IO[List[IO[Test]]] =
    List(
      showError,
      showFailure,
      showSkip,
      showSuccess,
      showGroupRootWithLabelSuccess,
      showGroupNestedWithoutLabelSuccess,
      showGroupNestedWithRepeatedLabelSuccess,
      showGroupNestedWithLabelSuccess
    ).map(IO.pure).pure[IO]
}
