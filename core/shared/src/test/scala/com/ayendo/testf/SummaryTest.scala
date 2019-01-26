package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf.internal.{Formatter, Text}
import sourcecode.Name

object SummaryTest extends TestF {
  def showSummary(summary: Summary)(implicit name: Name): Test[Id, String] =
    Test(name.value, Formatter.summary(summary, color = false))

  val showError: Assert[Id] =
    showSummary(Summary.Error("error", "reason")).equal("""✗ error
                                                          |  reason""".stripMargin)

  val showFailure: Assert[Id] = {
    val exception = new Exception("exception")

    val details = Text.padLeft(Formatter.throwable(exception), columns = 2)

    showSummary(Summary.Failure("failure", exception)).equal(s"""⚡failure
                                                                |$details""".stripMargin)
  }

  val showSuccess: Assert[Id] =
    showSummary(Summary.Success("success")).equal("✓ success")

  val showGroupRootWithoutDescriptionSuccess: Assert[Id] = {
    val group: Summary = Summary.Group(
      List(Summary.Success("s1"), Summary.Success("s2")),
      description = None
    )

    showSummary(group).equal("""✓ s1
                               |✓ s2""".stripMargin)
  }

  val showGroupRootWithDescriptionSuccess: Assert[Id] = {
    val group: Summary = Summary.Group(
      List(Summary.Success("s1"), Summary.Success("s2")),
      description = Some("success")
    )

    showSummary(group).equal("✓ success")
  }

  val showGroupRootWithRepeatedDescriptionsSuccess: Assert[Id] = {
    val group: Summary = Summary.Group(
      List(Summary.Success("s1"), Summary.Success("s1")),
      description = None
    )

    showSummary(group).equal("""✓ s1
                               |✓ s1""".stripMargin)
  }

//  val showGroupWithDescriptionSuccess: Assert = {
//    val group: Summary = Summary.Group(
//      Summary.Success("s1"),
//      Summary.Success("s2"),
//      description = Some("group")
//    )
//
//    showSummary(group).equal("✓ group")
//  }
//
//  val showGroupWithoutDescriptionError: Assert = {
//    val group: Summary = Summary.Group(
//      Summary.Success("success"),
//      Summary.Error("error", "reason"),
//      description = None
//    )
//
//    showSummary(group).equal("""✗ success |+| error
//                  |  ✓ success
//                  |  ✗ error
//                  |    reason""".stripMargin)
//  }
//
//  val showGroupWithDescriptionError: Assert = {
//    val group: Summary = Summary.Group(
//      Summary.Success("success"),
//      Summary.Error("error", "reason"),
//      description = Some("group")
//    )
//
//    showSummary(group).equal("""✗ group
//                  |  ✓ success
//                  |  ✗ error
//                  |    reason""".stripMargin)
//  }
//
//  val showNestedGroupWithoutDescriptionError: Assert = {
//    val group: Summary = Summary.Group(
//      Summary.Group(Summary.Success("s1"),
//                    Summary.Success("s2"),
//                    description = None),
//      Summary.Error("error", "reason"),
//      description = None
//    )
//
//    showSummary(group).equal(
//      """✗ s1 |+| s2 |+| error
//        |  ✓ s1
//        |  ✓ s2
//        |  ✗ error
//        |    reason""".stripMargin
//    )
//  }
//
//  val showNestedGroupWithDescriptionError: Assert = {
//    val group: Summary = Summary.Group(
//      Summary.Group(Summary.Success("s1"),
//                    Summary.Success("s2"),
//                    description = Some("g1")),
//      Summary.Error("error", "reason"),
//      description = None
//    )
//
//    showSummary(group).equal("""✗ g1 |+| error
//                  |  ✓ g1
//                  |  ✗ error
//                  |    reason""".stripMargin)
//  }

//  override val suite: List[Assert] = List(
//    showError,
//    showFailure,
//    showSuccess,
//    showGroupWithoutDescriptionSuccess,
//    showGroupWithRepeatedDescriptionsSuccess,
//    showGroupWithDescriptionSuccess,
//    showGroupWithoutDescriptionError,
//    showGroupWithDescriptionError,
//    showNestedGroupWithoutDescriptionError,
//    showNestedGroupWithDescriptionError
//  )

  override val suite: Assert[IO] =
    (
      showError |+|
        showFailure |+|
        showGroupRootWithoutDescriptionSuccess |+|
        showGroupRootWithDescriptionSuccess |+|
        showGroupRootWithRepeatedDescriptionsSuccess
    ).liftIO
}
