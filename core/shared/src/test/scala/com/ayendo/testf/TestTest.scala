package com.ayendo.testf

import cats.effect.IO
import com.ayendo.testf.internal.{Formatter, Text}

object TestTest extends TestF {
  def showTest(
      description: String,
      test: Test[Pure],
      expected: String
  ): Test[Pure] = {
    val value = Formatter.test(test, color = false)

    Test.label(
      description,
      Test.assert(
        value == expected,
        s"'$value' did not match expected '$expected'"
      )
    )
  }

  val showError: Test[Pure] =
    showTest("error", Test.error("reason"), """✗ error
                                              |  reason""".stripMargin)

  val showFailure: Test[Pure] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showTest("failure", Test.failure(exception), s"""⚡failure
                                                    |$details""".stripMargin)
  }

  val showSuccess: Test[Pure] =
    showTest("success", Test.success, "✓ success")
//
//  val showGroupRootWithLabelSuccess: Test[Id] = {
//    val test = Test.label(
//      "success",
//      Test.success("s1") |+| Test.success("s2")
//    )
//
//    showTest(test, "✓ success")
//  }
//
//  val showGroupNestedWithoutLabelSuccess: Test[Id] = {
//    val test = Test.group(Test.success("s1") |+| Test.success("s2"))
//
//    showTest(test, "✓ s1 |+| s2")
//  }
//
//  val showGroupNestedWithRepeatedLabelSuccess: Test[Id] = {
//    val test = Test.group(Test.success("s1") |+| Test.success("s1"))
//
//    showTest(test, "✓ s1")
//  }
//
//  val showGroupNestedWithLabelSuccess: Test[Id] = {
//    val test = Test.group(
//      Test.label("group", Test.group(Test.success("s1") |+| Test.success("s2")))
//    )
//
//    showTest(test, "✓ group")
//  }
//
//  override val suite: IO[List[Test.Result]] =
//    List(
//      showError,
//      showFailure,
//      showSuccess,
//      showGroupRootWithLabelSuccess,
//      showGroupNestedWithoutLabelSuccess,
//      showGroupNestedWithRepeatedLabelSuccess,
//      showGroupNestedWithLabelSuccess
//    ).map(_.compile).pure[IO]

  override val suite: IO[Test[Pure]] =
    Test.label("TestTest", Test.of(showError, showFailure, showSuccess)).compile
}
