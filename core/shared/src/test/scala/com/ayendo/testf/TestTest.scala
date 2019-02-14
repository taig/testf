package com.ayendo.testf

import cats.Id
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object TestTest extends TestF {
  def showTest(test: Test[Id, _], expected: String)(
      implicit name: Name): Test[Id, Unit] =
    Test.equal(name.value, Formatter.test(test, color = false), expected)

  val showError: Test[Id, Unit] =
    showTest(Test.error("error", "reason"),
             """✗ error
               |  reason""".stripMargin)

  val showFailure: Test[Id, Unit] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest(Test.failure("failure", exception),
             s"""⚡failure
                |$details""".stripMargin)
  }

  val showSkip: Test[Id, Unit] =
    showTest(Test.skip(Test.unit), "~ skip")

  val showSuccess: Test[Id, Unit] =
    showTest(Test.success("success", "foobar"), "✓ success")

  val showGroupRootWithoutLabelSuccess: Test[Id, Unit] = {
    val test = Test.unit[Id]("s1") |+| Test.unit[Id]("s2")

    showTest(test,
             """✓ s1
               |✓ s2""".stripMargin)
  }

  val showGroupRootWithLabelSuccess: Test[Id, Unit] = {
    val test = Test.label(
      "success",
      Test.unit[Id]("s1") |+| Test.unit[Id]("s2"),
    )

    showTest(test, "✓ success")
  }

  val showGroupRootWithRepeatedLabelSuccess: Test[Id, Unit] = {
    val test = Test.unit[Id]("s1") |+| Test.unit[Id]("s1")

    showTest(test,
             """✓ s1
               |✓ s1""".stripMargin)
  }

  val showGroupNestedWithoutLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s2"))

    showTest(test, "✓ s1 |+| s2")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s1"))

    showTest(test, "✓ s1")
  }

  val showGroupNestedWithLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(
      Test.label("group",
                 Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s2"))))

    showTest(test, "✓ group")
  }

  val showGroupNestedWithoutLabelError: Test[Id, Unit] = {
    val test = Test.group(
      Test.success[Id, String]("success") |+|
        Test.error[Id]("error", "reason"))

    showTest(test,
             """✗ success |+| error
                     |  ✓ success
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupNestedWithLabelError: Test[Id, Unit] = {
    val test = Test.group(
      Test.label("group",
                 Test.success[Id, String]("success") |+| Test
                   .error[Id]("error", "reason")))

    showTest(test,
             """✗ group
                     |  ✓ success
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedError: Test[Id, Unit] = {
    val test = Test.group(
      Test.group(
        Test.unit[Id]("s1") |+| Test.unit[Id]("s2"),
        Test.unit[Id]("s3") |+| Test.error("error", "reason")
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

  val showGroupDeeplyNestedWithoutLabelError: Test[Id, Unit] = {
    val test = Test.group(
      Test.group(
        Test.unit[Id]("s1") |+| Test.unit[Id]("s2"),
        Test.error("error", "reason"),
      )
    )

    showTest(test,
             """✗ s1 |+| s2 |+| error
                     |  ✓ s1 |+| s2
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedWithDescriptionError: Test[Id, Unit] = {
    val test = Test.group(
      Test.group(
        Test.label("g1", Test.unit[Id]("s1") |+| Test.unit[Id]("s2")),
        Test.error("error", "reason"),
      )
    )

    showTest(test,
             """✗ g1 |+| error
                     |  ✓ g1
                     |  ✗ error
                     |    reason""".stripMargin)
  }

  override def suite: Test[IO, Unit] = {
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

    tests.reduceLeft(_ |+| _).mapK(Test.liftId)
  }
}
