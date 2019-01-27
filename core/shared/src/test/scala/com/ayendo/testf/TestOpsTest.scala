package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    pure[Id, Boolean]("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    pure[Id, Boolean]("booleanTrue", true)

  val booleanFalseIsFalse: Assert[Id] =
    pure[Id, Assert[Id]]("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(success("booleanFalse"))

  val booleanFalseIsTrue: Assert[Id] =
    pure[Id, Assert[Id]]("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(error("booleanFalse", "false"))

  val booleanTrueIsFalse: Assert[Id] =
    pure[Id, Assert[Id]]("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(error("booleanTrue", "true"))

  val booleanTrueIsTrue: Assert[Id] =
    pure[Id, Assert[Id]]("booleanTrueIsTrue", booleanTrue.isTrue)
      .equal(success("booleanTrue"))

  val boolean: Test[Id, Assertion] =
    label(
      "boolean",
      booleanFalseIsFalse |+| booleanTrueIsFalse |+| booleanFalseIsTrue |+| booleanTrueIsTrue)

  val monoidEmpty: Test[Id, Option[Int]] =
    pure[Id, Option[Int]]("monoidEmpty", None)

  val monoidDefined: Test[Id, Option[Int]] =
    pure[Id, Option[Int]]("monoidDefined", Some(3))

  val monoidEmptyNonEmpty: Assert[Id] =
    pure[Id, Assert[Id]]("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Assert[Id] =
    pure[Id, Assert[Id]]("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(success("monoidEmpty"))

  val monoidDefinedNonEmpty: Assert[Id] =
    pure[Id, Assert[Id]]("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(success("monoidDefined"))

  val monoidDefinedIsEmpty: Assert[Id] =
    pure[Id, Assert[Id]]("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(error("monoidDefined", "not empty Some(3)"))

  val monoid: Assert[Id] =
    label("option",
          monoidEmptyNonEmpty |+|
            monoidEmptyIsEmpty |+|
            monoidDefinedNonEmpty |+|
            monoidDefinedIsEmpty)

  override val suite: Assert[IO] = (boolean |+| monoid).liftIO
}
