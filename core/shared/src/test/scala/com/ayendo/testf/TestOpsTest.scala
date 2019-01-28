package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    value("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    value("booleanTrue", true)

  val booleanFalseIsFalse: Test[Id, Assertion] =
    value("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(success("booleanFalse"))

  val booleanFalseIsTrue: Test[Id, Assertion] =
    value("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(error("booleanFalse", "false"))

  val booleanTrueIsFalse: Test[Id, Assertion] =
    value("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(error("booleanTrue", "true"))

  val booleanTrueIsTrue: Test[Id, Assertion] =
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

  val monoidEmptyNonEmpty: Test[Id, Assertion] =
    value("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Test[Id, Assertion] =
    value("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(success("monoidEmpty"))

  val monoidDefinedNonEmpty: Test[Id, Assertion] =
    value("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(success("monoidDefined"))

  val monoidDefinedIsEmpty: Test[Id, Assertion] =
    value("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(error("monoidDefined", "not empty Some(3)"))

  val monoid: Test[Id, Assertion] =
    label("option",
          monoidEmptyNonEmpty |+|
            monoidEmptyIsEmpty |+|
            monoidDefinedNonEmpty |+|
            monoidDefinedIsEmpty)

  override val suite: Test[IO, Assertion] = (boolean |+| monoid).liftIO
}
