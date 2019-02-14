package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object TestTest extends TestF {
  def showTest(test: Test[Id, _], expected: String)(
      implicit name: Name): Test[Id, Unit] =
    Test.equal(name.value,
               Formatter.test(test, duration = 0, color = false),
               expected)

  val showError: Test[Id, Unit] =
    showTest(Test.error("error", "reason"),
             """✗ error (0ms)
               |  reason""".stripMargin)

  val showFailure: Test[Id, Unit] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest(Test.failure("failure", exception),
             s"""⚡failure (0ms)
                |$details""".stripMargin)
  }

  val showSkip: Test[Id, Unit] =
    showTest(Test.skip(Test.unit), "~ skip")

  val showSuccess: Test[Id, Unit] =
    showTest(Test.success("success", "foobar"), "✓ success (0ms)")

  val showGroupRootWithLabelSuccess: Test[Id, Unit] = {
    val test = Test.label(
      "success",
      Test.unit[Id]("s1") |+| Test.unit[Id]("s2"),
    )

    showTest(test, "✓ success (0ms)")
  }

  val showGroupNestedWithoutLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s2"))

    showTest(test, "✓ s1 |+| s2 (0ms)")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s1"))

    showTest(test, "✓ s1 (0ms)")
  }

  val showGroupNestedWithLabelSuccess: Test[Id, Unit] = {
    val test = Test.group(
      Test.label("group",
                 Test.group(Test.unit[Id]("s1") |+| Test.unit[Id]("s2"))))

    showTest(test, "✓ group (0ms)")
  }

  override val suite: List[Test[IO, Unit]] =
    List(
      showError,
      showFailure,
      showSkip,
      showSuccess,
      showGroupRootWithLabelSuccess,
      showGroupNestedWithoutLabelSuccess,
      showGroupNestedWithRepeatedLabelSuccess,
      showGroupNestedWithLabelSuccess
    ).map(_.mapK(Test.liftId))
}
