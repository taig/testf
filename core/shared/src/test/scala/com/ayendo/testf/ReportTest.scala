package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object ReportTest extends TestF {
  def showReport(report: Report)(implicit name: Name): Test[Id, String] =
    Test.pure(name.value, Formatter.report(report, color = false))

  val showError: Test[Id, Unit] =
    showReport(Report.error("error", "reason"))
      .equal("""✗ error
               |  reason""".stripMargin)

  val showFailure: Test[Id, Unit] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showReport(Report.failure("failure", exception))
      .equal(s"""⚡failure
                |$details""".stripMargin)
  }

  val showSkip: Test[Id, Unit] =
    showReport(Report.skip("skip")).equal("~ skip")

  val showSuccess: Test[Id, Unit] =
    showReport(Report.success("success")).equal("✓ success")

  val showGroupRootWithoutLabelSuccess: Test[Id, Unit] = {
    val report = Report.success("s1") |+| Report.success("s2")

    showReport(report).equal("""✓ s1
                               |✓ s2""".stripMargin)
  }

  val showGroupRootWithLabelSuccess: Test[Id, Unit] = {
    val report = Report.label(
      "success",
      Report.success("s1") |+| Report.success("s2"),
    )

    showReport(report).equal("✓ success")
  }

  val showGroupRootWithRepeatedLabelSuccess: Test[Id, Unit] = {
    val report = Report.success("s1") |+| Report.success("s1")

    showReport(report).equal("""✓ s1
                               |✓ s1""".stripMargin)
  }

  val showGroupNestedWithoutLabelSuccess: Test[Id, Unit] = {
    val report = Report.group(Report.success("s1") |+| Report.success("s2"))

    showReport(report).equal("✓ s1 |+| s2")
  }

  val showGroupNestedWithRepeatedLabelSuccess: Test[Id, Unit] = {
    val report = Report.group(Report.success("s1") |+| Report.success("s1"))

    showReport(report).equal("✓ s1")
  }

  val showGroupNestedWithLabelSuccess: Test[Id, Unit] = {
    val report = Report.group(
      Report.label("group",
                   Report.group(Report.success("s1") |+| Report.success("s2"))))

    showReport(report).equal("✓ group")
  }

  val showGroupNestedWithoutLabelError: Test[Id, Unit] = {
    val report = Report.group(
      Report.success("success") |+| Report.error("error", "reason"))

    showReport(report).equal("""✗ success |+| error
                               |  ✓ success
                               |  ✗ error
                               |    reason""".stripMargin)
  }

  val showGroupNestedWithLabelError: Test[Id, Unit] = {
    val report = Report.group(
      Report.label(
        "group",
        Report.success("success") |+| Report.error("error", "reason")))

    showReport(report).equal("""✗ group
                               |  ✓ success
                               |  ✗ error
                               |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedError: Test[Id, Unit] = {
    val report = Report.group(
      Report.group(
        Report.success("s1") |+| Report.success("s2"),
        Report.error("error", "reason") |+| Report.success("s3")
      )
    )

    showReport(report).equal(
      """✗ s1 |+| s2 |+| error |+| s3
        |  ✓ s1 |+| s2
        |  ✗ error |+| s3
        |    ✗ error
        |      reason
        |    ✓ s3""".stripMargin
    )
  }

  val showGroupDeeplyNestedWithoutLabelError: Test[Id, Unit] = {
    val report = Report.group(
      Report.group(
        Report.success("s1") |+| Report.success("s2"),
        Report.error("error", "reason"),
      )
    )

    showReport(report).equal(
      """✗ s1 |+| s2 |+| error
        |  ✓ s1 |+| s2
        |  ✗ error
        |    reason""".stripMargin
    )
  }

  val showGroupDeeplyNestedWithDescriptionError: Test[Id, Unit] = {
    val report = Report.group(
      Report.group(
        Report.label("g1", Report.success("s1") |+| Report.success("s2")),
        Report.error("error", "reason"),
      )
    )

    showReport(report).equal(
      """✗ g1 |+| error
        |  ✓ g1
        |  ✗ error
        |    reason""".stripMargin
    )
  }

  override val suite: Test[IO, Unit] =
    (
      showError |+|
        showFailure |+|
        showSkip |+|
        showSuccess |+|
        showGroupRootWithoutLabelSuccess |+|
        showGroupRootWithLabelSuccess |+|
        showGroupRootWithRepeatedLabelSuccess |+|
        showGroupNestedWithoutLabelSuccess |+|
        showGroupNestedWithRepeatedLabelSuccess |+|
        showGroupNestedWithLabelSuccess |+|
        showGroupNestedWithoutLabelError |+|
        showGroupNestedWithLabelError |+|
        showGroupDeeplyNestedError |+|
        showGroupDeeplyNestedWithoutLabelError |+|
        showGroupDeeplyNestedWithDescriptionError
    ).liftIO
}
