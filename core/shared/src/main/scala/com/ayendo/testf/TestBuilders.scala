package com.ayendo.testf

import cats._
import cats.implicits._

trait TestBuilders {
  def apply[A](label: String, value: A): Test[A] =
    this.success(label, value)

  def cond(predicate: Boolean): Test[Unit] =
    if (predicate) this.unit else this.error("false")

  def cond(label: String, predicate: Boolean): Test[Unit] =
    this.label(label, this.cond(predicate))

  def equal[A: Eq: Show](actual: A, expected: A): Test[Unit] =
    if (actual === expected) this.unit
    else this.error(show"$actual does not match expected $expected")

  def equal[A: Eq: Show](label: String, actual: A, expected: A): Test[Unit] =
    this.label(label, this.equal(actual, expected))

  def group[A](tests: Test[A]*): Test[A] = Test.Group(tests.toList)

  def label[A](label: String, test: Test[A]): Test[A] =
    Test.Label(label, test)

  def message[A](label: String, test: Test[A]): Test[A] =
    Test.Message(label, test)

  def not[A](test: Test[A]): Test[Unit] =
    if (test.success) test.flatMap(_ => this.error("not(success)"))
    else test.flatMap(_ => this.unit)

  def pure[A](value: A): Test[A] = Test.Success(value)

  def success[A](value: A): Test[A] = Test.Success(value)

  def success[A](label: String, value: A): Test[A] =
    this.label(label, this.success(value))

  def error(message: String): Test[Nothing] = Test.Error(message)

  def error(label: String, message: String): Test[Nothing] =
    this.label(label, this.error(message))

  def failure(label: String, throwable: Throwable): Test[Nothing] =
    this.label(label, Test.Failure(throwable))

  def skip[A](test: Test[A]): Test[A] = Test.Skip(test)

  val unit: Test[Unit] = success(())

  def unit(label: String): Test[Unit] = this.success(label, ())
}
