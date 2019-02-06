package com.ayendo.testf

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object TestTest extends TestF {
  def showTest(test: Test[_], expected: String)(
      implicit name: Name): Test[Unit] =
    Test.equal(name.value, Formatter.test(test, color = false), expected)

  val showError: Test[Unit] =
    showTest(Test.error("error", "reason"),
             """✗ error
               |  reason""".stripMargin)

  val showFailure: Test[Unit] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest(Test.failure("failure", exception),
             s"""⚡failure
                |$details""".stripMargin)
  }

  val showSkip: Test[Unit] =
    showTest(Test.skip(Test.unit), "~ skip")

  val showSuccess: Test[Unit] =
    showTest(Test.success("success", "foobar"), "✓ success")

  val showGroupRootWithoutLabelSuccess: Test[Unit] = {
    val test = Test.unit("s1") |+| Test.unit("s2")

    showTest(test,
             """✓ s1
               |✓ s2""".stripMargin)
  }

  val showGroupRootWithLabelSuccess: Test[Unit] = {
    val test = Test.label(
      "success",
      Test.unit("s1") |+| Test.unit("s2"),
    )

    showTest(test, "✓ success")
  }

  val showGroupRootWithRepeatedLabelSuccess: Test[Unit] = {
    val test = Test.unit("s1") |+| Test.unit("s1")

    showTest(test,
             """✓ s1
               |✓ s1""".stripMargin)
  }

  val showGroupNestedWithoutLabelSuccess: Test[Unit] = {
    val test = Test.group(Test.unit("s1") |+| Test.unit("s2"))

    showTest(test, "✓ s1 |+| s2")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test[Unit] = {
    val test = Test.group(Test.unit("s1") |+| Test.unit("s1"))

    showTest(test, "✓ s1")
  }

  val showGroupNestedWithLabelSuccess: Test[Unit] = {
    val test = Test.group(
      Test.label("group", Test.group(Test.unit("s1") |+| Test.unit("s2"))))

    showTest(test, "✓ group")
  }

  val showGroupNestedWithoutLabelError: Test[Unit] = {
    val test =
      Test.group(Test.success("success") |+| Test.error("error", "reason"))

    showTest(test,
             """✗ success |+| error
                     |  ✓ success
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupNestedWithLabelError: Test[Unit] = {
    val test = Test.group(
      Test.label("group",
                 Test.success("success") |+| Test.error("error", "reason")))

    showTest(test,
             """✗ group
                     |  ✓ success
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedError: Test[Unit] = {
    val test = Test.group(
      Test.group(
        Test.unit("s1") |+| Test.unit("s2"),
        Test.unit("s3") |+| Test.error("error", "reason")
      )
    )

    showTest(
      test,
      """✗ s1 |+| s2 |+| s3 |+| error
        |  ✓ s1 |+| s2
        |  ✗ s3 |+| error
        |    ✓ s3
        |    ✗ error
        |      reason""".stripMargin
    )
  }

  val showGroupDeeplyNestedWithoutLabelError: Test[Unit] = {
    val test = Test.group(
      Test.group(
        Test.unit("s1") |+| Test.unit("s2"),
        Test.error("error", "reason"),
      )
    )

    showTest(test,
             """✗ s1 |+| s2 |+| error
                     |  ✓ s1 |+| s2
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedWithDescriptionError: Test[Unit] = {
    val test = Test.group(
      Test.group(
        Test.label("g1", Test.unit("s1") |+| Test.unit("s2")),
        Test.error("error", "reason"),
      )
    )

    showTest(test,
             """✗ g1 |+| error
                     |  ✓ g1
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  override val suite: IO[Test[Unit]] = {
    val tests = NonEmptyList.of(
      showError,
      showFailure,
      showSkip,
      showSuccess,
      showGroupRootWithoutLabelSuccess,
      showGroupRootWithLabelSuccess,
      showGroupRootWithRepeatedLabelSuccess,
      showGroupNestedWithoutLabelSuccess,
      showGroupNestedWithRepeatedLabelSuccess,
      showGroupNestedWithLabelSuccess,
      showGroupNestedWithoutLabelError,
      showGroupNestedWithLabelError,
      showGroupDeeplyNestedError,
      showGroupDeeplyNestedWithoutLabelError,
      showGroupDeeplyNestedWithDescriptionError
    )
    IO.pure(tests.reduceLeft(_ |+| _))
  }
}
