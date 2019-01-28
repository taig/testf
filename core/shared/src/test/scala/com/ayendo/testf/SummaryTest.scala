package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object ReportTest extends TestF {
  def showReport(report: Report)(implicit name: Name): Test[Id, String] =
    value(name.value, Formatter.report(report, color = false))

  val showError: Assert[Id] =
    showReport(Report.Error("error", "reason")).equal("""✗ error
                                                          |  reason""".stripMargin)

  val showFailure: Assert[Id] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showReport(Report.Failure("failure", exception)).equal(s"""⚡failure
                                                                |$details""".stripMargin)
  }

  val showSkip: Assert[Id] =
    showReport(Report.Skip("skip")).equal("~ skip")

  val showSuccess: Assert[Id] =
    showReport(Report.Success("success")).equal("✓ success")

  val showGroupRootWithoutDescriptionSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(Report.Success("s1"), Report.Success("s2")),
      description = None
    )

    showReport(group).equal("""✓ s1
                               |✓ s2""".stripMargin)
  }

  val showGroupRootWithDescriptionSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(Report.Success("s1"), Report.Success("s2")),
      description = Some("success")
    )

    showReport(group).equal("✓ success")
  }

  val showGroupRootWithRepeatedDescriptionsSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(Report.Success("s1"), Report.Success("s1")),
      description = None
    )

    showReport(group).equal("""✓ s1
                               |✓ s1""".stripMargin)
  }

  val showGroupNestedWithoutDescriptionSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(List(Report.Success("s1"), Report.Success("s2")),
                     description = None)),
      description = None)

    showReport(group).equal("✓ s1 |+| s2")
  }

  val showGroupNestedWithRepeatedDescriptionSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(List(Report.Success("s1"), Report.Success("s1")),
                     description = None)),
      description = None)

    showReport(group).equal("✓ s1")
  }

  val showGroupNestedWithDescriptionSuccess: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(List(Report.Success("s1"), Report.Success("s2")),
                     description = Some("group"))),
      description = None)

    showReport(group).equal("✓ group")
  }

  val showGroupNestedWithoutDescriptionError: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(List(Report.Success("success"),
                          Report.Error("error", "reason")),
                     description = None)),
      description = None
    )

    showReport(group).equal("""✗ success |+| error
                               |  ✓ success
                               |  ✗ error
                               |    reason""".stripMargin)
  }

  val showGroupNestedWithDescriptionError: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(List(Report.Success("success"),
                          Report.Error("error", "reason")),
                     description = Some("group"))),
      description = None)

    showReport(group).equal("""✗ group
                  |  ✓ success
                  |  ✗ error
                  |    reason""".stripMargin)
  }

  val showGroupDeeplyNestedError: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(
          List(
            Report.Group(List(Report.Success("s1"), Report.Success("s2")),
                         description = None),
            Report.Group(List(Report.Error("error", "reason"),
                              Report.Success("s3")),
                         description = None)
          ),
          description = None
        )),
      description = None
    )

    showReport(group).equal(
      """✗ s1 |+| s2 |+| error |+| s3
        |  ✓ s1 |+| s2
        |  ✗ error |+| s3
        |    ✗ error
        |      reason
        |    ✓ s3""".stripMargin
    )
  }

  val showGroupDeeplyNestedWithoutDescriptionError: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(
          List(
            Report.Group(List(Report.Success("s1"), Report.Success("s2")),
                         description = None),
            Report.Error("error", "reason"),
          ),
          description = None
        )),
      description = None
    )

    showReport(group).equal(
      """✗ s1 |+| s2 |+| error
        |  ✓ s1 |+| s2
        |  ✗ error
        |    reason""".stripMargin
    )
  }

  val showGroupDeeplyNestedWithDescriptionError: Assert[Id] = {
    val group: Report = Report.Group(
      List(
        Report.Group(
          List(
            Report.Group(List(Report.Success("s1"), Report.Success("s2")),
                         description = Some("g1")),
            Report.Error("error", "reason"),
          ),
          description = None
        )),
      description = None
    )

    showReport(group).equal(
      """✗ g1 |+| error
        |  ✓ g1
        |  ✗ error
        |    reason""".stripMargin
    )
  }

  override val suite: Assert[IO] =
    (
      showError |+|
        showFailure |+|
        showSkip |+|
        showSuccess |+|
        showGroupRootWithoutDescriptionSuccess |+|
        showGroupRootWithDescriptionSuccess |+|
        showGroupRootWithRepeatedDescriptionsSuccess |+|
        showGroupNestedWithDescriptionSuccess |+|
        showGroupNestedWithRepeatedDescriptionSuccess |+|
        showGroupNestedWithoutDescriptionSuccess |+|
        showGroupNestedWithoutDescriptionError |+|
        showGroupNestedWithDescriptionError |+|
        showGroupDeeplyNestedError |+|
        showGroupDeeplyNestedWithoutDescriptionError |+|
        showGroupDeeplyNestedWithDescriptionError
    ).liftIO
}
