package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    value("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    value("booleanTrue", true)

  val booleanFalseIsFalse: Assert[Id] =
    value("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(success("booleanFalse"))

  val booleanFalseIsTrue: Assert[Id] =
    value("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(error("booleanFalse", "false"))

  val booleanTrueIsFalse: Assert[Id] =
    value("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(error("booleanTrue", "true"))

  val booleanTrueIsTrue: Assert[Id] =
    value("booleanTrueIsTrue", booleanTrue.isTrue)
      .equal(success("booleanTrue"))

  val boolean: Test[Id, Assertion] =
    label(
      "boolean",
      booleanFalseIsFalse |+| booleanTrueIsFalse |+| booleanFalseIsTrue |+| booleanTrueIsTrue)

  val monoidEmpty: Test[Id, Option[Int]] =
    value("monoidEmpty", None)

  val monoidDefined: Test[Id, Option[Int]] =
    value("monoidDefined", Some(3))

  val monoidEmptyNonEmpty: Assert[Id] =
    value("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Assert[Id] =
    value("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(success("monoidEmpty"))

  val monoidDefinedNonEmpty: Assert[Id] =
    value("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(success("monoidDefined"))

  val monoidDefinedIsEmpty: Assert[Id] =
    value("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(error("monoidDefined", "not empty Some(3)"))

  val monoid: Assert[Id] =
    label("option",
          monoidEmptyNonEmpty |+|
            monoidEmptyIsEmpty |+|
            monoidDefinedNonEmpty |+|
            monoidDefinedIsEmpty)

  override val suite: Assert[IO] = (boolean |+| monoid).liftIO
}
