package com.ayendo.testf

import cats.Id
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object SummaryTest extends TestF {
  def showSummary(summary: Summary, expected: String)(
      implicit name: Name): Test =
    equal[Id, String](name.value,
                      Formatter.summary(summary, color = false),
                      expected)

  val showError: Test =
    showSummary(Summary.Error("error", "reason"),
                """✗ error
                  |  reason""".stripMargin)

  val showFailure: Test = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showSummary(
      Summary.Failure("failure", exception),
      s"""⚡failure
         |$details""".stripMargin
    )
  }

  val showSuccess: Test =
    showSummary(Summary.Success("success"), "✓ success")

  val showGroupWithoutDescriptionSuccess: Test = {
    val group: Summary = Summary.Group(
      Summary.Success("s1"),
      Summary.Success("s2"),
      description = None
    )

    showSummary(group, "✓ s1 |+| s2")
  }

  val showGroupWithRepeatedDescriptionsSuccess: Test = {
    val group: Summary = Summary.Group(
      Summary.Success("s1"),
      Summary.Success("s1"),
      description = None
    )

    showSummary(group, "✓ s1")
  }

  val showGroupWithDescriptionSuccess: Test = {
    val group: Summary = Summary.Group(
      Summary.Success("s1"),
      Summary.Success("s2"),
      description = Some("group")
    )

    showSummary(group, "✓ group")
  }

  val showGroupWithoutDescriptionError: Test = {
    val group: Summary = Summary.Group(
      Summary.Success("success"),
      Summary.Error("error", "reason"),
      description = None
    )

    showSummary(group,
                """✗ success |+| error
                  |  ✓ success
                  |  ✗ error
                  |    reason""".stripMargin)
  }

  val showGroupWithDescriptionError: Test = {
    val group: Summary = Summary.Group(
      Summary.Success("success"),
      Summary.Error("error", "reason"),
      description = Some("group")
    )

    showSummary(group,
                """✗ group
                  |  ✓ success
                  |  ✗ error
                  |    reason""".stripMargin)
  }

  val showNestedGroupWithoutDescriptionError: Test = {
    val group: Summary = Summary.Group(
      Summary.Group(Summary.Success("s1"),
                    Summary.Success("s2"),
                    description = None),
      Summary.Error("error", "reason"),
      description = None
    )

    showSummary(
      group,
      """✗ s1 |+| s2 |+| error
        |  ✓ s1
        |  ✓ s2
        |  ✗ error
        |    reason""".stripMargin
    )
  }

  val showNestedGroupWithDescriptionError: Test = {
    val group: Summary = Summary.Group(
      Summary.Group(Summary.Success("s1"),
                    Summary.Success("s2"),
                    description = Some("g1")),
      Summary.Error("error", "reason"),
      description = None
    )

    showSummary(group,
                """✗ g1 |+| error
                  |  ✓ g1
                  |  ✗ error
                  |    reason""".stripMargin)
  }

  override val suite: List[Test] = List(
    showError,
    showFailure,
    showSuccess,
    showGroupWithoutDescriptionSuccess,
    showGroupWithRepeatedDescriptionsSuccess,
    showGroupWithDescriptionSuccess,
    showGroupWithoutDescriptionError,
    showGroupWithDescriptionError,
    showNestedGroupWithoutDescriptionError,
    showNestedGroupWithDescriptionError
  )
}
