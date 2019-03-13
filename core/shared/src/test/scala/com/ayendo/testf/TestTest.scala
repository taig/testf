package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import com.ayendo.testf.implicits._
import sourcecode.Name

object TestTest extends TestF {
  def showTest(test: Test[Id], expected: String)(
      implicit name: Name): Test[Id] =
    name.value @@ Test.equal(
      Formatter.test(test, duration = None, color = false),
      expected)

  val showError: Test[Id] =
    showTest("error" @@ Test.error("reason"),
             """✗ error
               |  reason""".stripMargin)

  val showFailure: Test[Id] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest("failure" @@ Test.failure(exception),
             s"""⚡failure
                |$details""".stripMargin)
  }

  val showSuccess: Test[Id] =
    showTest(Test.success("success"), "✓ success")

  val showGroupRootWithLabelSuccess: Test[Id] = {
    val test = Test.label(
      "success",
      Test.success("s1") |+| Test.success("s2"),
    )

    showTest(test, "✓ success")
  }

  val showGroupNestedWithoutLabelSuccess: Test[Id] = {
    val test = Test.group(Test.success("s1") |+| Test.success("s2"))

    showTest(test, "✓ s1 |+| s2")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test[Id] = {
    val test = Test.group(Test.success("s1") |+| Test.success("s1"))

    showTest(test, "✓ s1")
  }

  val showGroupNestedWithLabelSuccess: Test[Id] = {
    val test = Test.group(
      Test.label("group",
                 Test.group(Test.success("s1") |+| Test.success("s2"))))

    showTest(test, "✓ group")
  }

  override val suite: IO[List[Test.Result]] =
    List(
      showError,
      showFailure,
      showSuccess,
      showGroupRootWithLabelSuccess,
      showGroupNestedWithoutLabelSuccess,
      showGroupNestedWithRepeatedLabelSuccess,
      showGroupNestedWithLabelSuccess
    ).map(_.compile).pure[IO]
}
