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

  val monoidEmpty: Test[Id, Option[Int]] =
    Test.pure[Id, Option[Int]]("monoidEmpty", None)

  val monoidDefined: Test[Id, Option[Int]] =
    Test.pure[Id, Option[Int]]("monoidDefined", Some(3))

  val monoidEmptyNonEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(Test.error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(Test.success("monoidEmpty"))

  val monoidDefinedNonEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(Test.success("monoidDefined"))

  val monoidDefinedIsEmpty: Assert[Id] =
    Test
      .pure[Id, Assert[Id]]("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(Test.error("monoidDefined", "not empty Some(3)"))

  val monoid: Assert[Id] =
    Test.label("option",
               monoidEmptyNonEmpty |+|
                 monoidEmptyIsEmpty |+|
                 monoidDefinedNonEmpty |+|
                 monoidDefinedIsEmpty)

  override val suite: Assert[IO] = (boolean |+| monoid).liftIO
}
