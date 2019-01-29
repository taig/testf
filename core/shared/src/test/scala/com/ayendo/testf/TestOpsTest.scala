package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    value("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    value("booleanTrue", true)

  val booleanFalseIsFalse: Test[Id, Unit] =
    value("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(success("booleanFalse"))

  val booleanFalseIsTrue: Test[Id, Unit] =
    value("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(error("booleanFalse", "false"))

  val booleanTrueIsFalse: Test[Id, Unit] =
    value("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(error("booleanTrue", "true"))

  val booleanTrueIsTrue: Test[Id, Unit] =
    value("booleanTrueIsTrue", booleanTrue.isTrue)
      .equal(success("booleanTrue"))

  val boolean: Test[Id, Unit] =
    label(
      "boolean",
      booleanFalseIsFalse |+| booleanTrueIsFalse |+| booleanFalseIsTrue |+| booleanTrueIsTrue)

  val monoidEmpty: Test[Id, Option[Int]] =
    value("monoidEmpty", None)

  val monoidDefined: Test[Id, Option[Int]] =
    value("monoidDefined", Some(3))

  val monoidEmptyNonEmpty: Test[Id, Unit] =
    value("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Test[Id, Unit] =
    value("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(success("monoidEmpty"))

  val monoidDefinedNonEmpty: Test[Id, Unit] =
    value("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(success("monoidDefined"))

  val monoidDefinedIsEmpty: Test[Id, Unit] =
    value("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(error("monoidDefined", "not empty Some(3)"))

  val monoid: Test[Id, Unit] =
    label("option",
          monoidEmptyNonEmpty |+|
            monoidEmptyIsEmpty |+|
            monoidDefinedNonEmpty |+|
            monoidDefinedIsEmpty)

  override val suite: Test[IO, Unit] = (boolean |+| monoid).liftIO
}
