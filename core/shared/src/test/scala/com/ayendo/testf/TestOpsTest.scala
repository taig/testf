package com.ayendo.testf

import cats.Id
import cats.effect.IO
import cats.implicits._

object TestOpsTest extends TestF {
  val booleanFalse: Test[Id, Boolean] =
    Test.pure("booleanFalse", false)

  val booleanTrue: Test[Id, Boolean] =
    Test.pure("booleanTrue", true)

  val booleanFalseIsFalse: Test[Id, Unit] =
    Test
      .pure("booleanFalseIsFalse", booleanFalse.isFalse)
      .equal(Test.success("booleanFalse", ()))

  val booleanFalseIsTrue: Test[Id, Unit] =
    Test
      .pure("booleanFalseIsTrue", booleanFalse.isTrue)
      .equal(Test.error("booleanFalse", "false"))

  val booleanTrueIsFalse: Test[Id, Unit] =
    Test
      .pure("booleanTrueIsFalse", booleanTrue.isFalse)
      .equal(Test.error("booleanTrue", "true"))

  val booleanTrueIsTrue: Test[Id, Unit] =
    Test
      .pure("booleanTrueIsTrue", booleanTrue.isTrue)
      .equal(Test.success("booleanTrue", ()))

  val boolean: Test[Id, Unit] =
    Test.label(
      "boolean",
      booleanFalseIsFalse |+| booleanTrueIsFalse |+| booleanFalseIsTrue |+| booleanTrueIsTrue)

  val monoidEmpty: Test[Id, Option[Int]] =
    Test.pure("monoidEmpty", None)

  val monoidDefined: Test[Id, Option[Int]] =
    Test.pure("monoidDefined", Some(3))

  val monoidEmptyNonEmpty: Test[Id, Unit] =
    Test
      .pure("monoidEmptyNonEmpty", monoidEmpty.nonEmpty)
      .equal(Test.error("monoidEmpty", "empty None"))

  val monoidEmptyIsEmpty: Test[Id, Unit] =
    Test
      .pure("monoidEmptyIsEmpty", monoidEmpty.isEmpty)
      .equal(Test.success("monoidEmpty", ()))

  val monoidDefinedNonEmpty: Test[Id, Unit] =
    Test
      .pure("monoidDefinedNonEmpty", monoidDefined.nonEmpty)
      .equal(Test.success("monoidDefined", ()))

  val monoidDefinedIsEmpty: Test[Id, Unit] =
    Test
      .pure("monoidDefinedIsEmpty", monoidDefined.isEmpty)
      .equal(Test.error("monoidDefined", "not empty Some(3)"))

  val monoid: Test[Id, Unit] =
    Test.label("option",
               monoidEmptyNonEmpty |+|
                 monoidEmptyIsEmpty |+|
                 monoidDefinedNonEmpty |+|
                 monoidDefinedIsEmpty)

  override val suite: Test[IO, Unit] = (boolean |+| monoid).liftIO
}
