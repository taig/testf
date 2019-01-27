package com.ayendo.testf

import cats.Functor
import cats.implicits._
import com.ayendo.testf.Test.{
  Error,
  Failure,
  Label,
  Pure,
  Skip,
  Success,
  Suspend
}

trait TestBuilders {
  def defer[F[_]: Functor, A](description: String, value: F[A]): Test[F, A] =
    label(description, Suspend[F, A](value.map(Pure.apply)))

  def label[F[_], A](description: String, test: Test[F, A]): Test[F, A] =
    Label(description, test)

  def pure[F[_], A](description: String, value: A): Test[F, A] =
    label(description, Pure(value))

  def success[F[_], A](description: String): Test[F, A] =
    label(description, Success())

  def error[F[_], A](description: String, message: String): Test[F, A] =
    label(description, Error(message))

  def failure[F[_], A](description: String, throwable: Throwable): Test[F, A] =
    label(description, Failure(throwable))

  def skip[F[_], A](test: Test[F, A]): Test[F, A] = Skip(test)

  def suspend[F[_]: Functor, A](test: F[Test[F, A]]): Test[F, A] = Suspend(test)
}
