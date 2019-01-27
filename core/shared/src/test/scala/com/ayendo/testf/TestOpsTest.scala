package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    Test.pure[Id, Boolean]("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    Test.pure[Id, Boolean]("booleanTrue", true)

  val booleanFalseIsFalse: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(Test.success("booleanFalse"))

  val booleanFalseIsTrue: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(Test.error("booleanFalse", "false"))

  val booleanTrueIsFalse: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(Test.error("booleanTrue", "true"))

  val booleanTrueIsTrue: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("booleanTrueIsTrue", booleanTrue.isTrue)
      .equal(Test.success("booleanTrue"))

  val boolean: Test[Id, Assertion] =
    Test.label(
      "boolean",
      booleanFalseIsFalse |+| booleanTrueIsFalse |+| booleanFalseIsTrue |+| booleanTrueIsTrue)

  val eitherLeft: Test[Id, Either[Int, Int]] =
    Test.pure[Id, Either[Int, Int]]("eitherLeft", Left(3))

  val eitherRight: Test[Id, Either[Int, Int]] =
    Test.pure[Id, Either[Int, Int]]("eitherRight", Right(3))

  val eitherLeftIsLeft: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("eitherLeftIsLeft", eitherLeft.isLeft)
      .equal(Test.success("eitherLeft"))

  val eitherLeftIsRight: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("eitherLeftIsRight", eitherLeft.isRight)
      .equal(Test.error("eitherLeft", "Either is Right(3)"))

  val eitherRightIsRight: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("eitherRightIsRight", eitherRight.isRight)
      .equal(Test.success("eitherRight"))

  val eitherRightIsLeft: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("eitherRightIsLeft", eitherRight.isLeft)
      .equal(Test.error("eitherRight", "Either is Left(3)"))

  val either: Assert[Id] = Test.label(
    "either",
    eitherRightIsLeft |+| eitherLeftIsRight |+| eitherRightIsRight |+| eitherLeftIsRight)

  val optionEmpty: Test[Id, Option[Int]] =
    Test.pure[Id, Option[Int]]("optionEmpty", None)

  val optionDefined: Test[Id, Option[Int]] =
    Test.pure[Id, Option[Int]]("optionDefined", Some(3))

  val optionEmptyIsDefined: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("optionEmptyIsDefined", optionEmpty.isDefined)
      .equal(Test.error("optionEmpty", "Option is None"))

  val optionEmptyIsEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("optionEmptyIsEmpty", optionEmpty.isEmpty)
      .equal(Test.success("optionEmpty"))

  val optionDefinedIsDefined: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("optionDefinedIsDefined", optionDefined.isDefined)
      .equal(Test.success("optionDefined"))

  val optionDefinedIsEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("optionDefinedIsEmpty", optionDefined.isEmpty)
      .equal(Test.error("optionDefined", "Option is Some(3)"))

  val option: Assert[Id] =
    Test.label(
      "option",
      optionEmptyIsDefined |+| optionEmptyIsEmpty |+| optionDefinedIsDefined |+| optionDefinedIsEmpty)

  override def suite: Assert[IO] =
    (
      boolean |+|
        either |+|
        option
    ).liftIO
}
