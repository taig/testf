package io.taig.testf

import cats._
import cats.data._
import cats.implicits._

trait Builders { self =>
  def and[F[_], A](tests: List[Test[F, A]]): Test[F, A] = Test.And(tests)

  val empty: Test[Pure, Nothing] = Test.And(List.empty)

  def error(message: String): Test[Pure, Nothing] = Test.Error(message)

  def eval[F[_]: Functor, A](value: F[A]): Test[F, A] =
    Test.Eval[F, A](value.map(pure))

  def eval[F[_]: Functor, A](description: String)(value: F[A]): Test[F, A] =
    label(description, eval(value))

  def failure(throwable: Throwable): Test[Pure, Nothing] =
    Test.Failure(throwable)

  def failure(description: String)(throwable: Throwable): Test[Pure, Nothing] =
    label(description, failure(throwable))

  def force[F[_], A](value: F[Test[F, A]]): Test[F, A] = Test.Eval[F, A](value)

  def force[F[_], A](description: String)(value: F[Test[F, A]]): Test[F, A] =
    label(description, force(value))

  def fromRight[A: Show, B](either: Either[A, B]): Test[Pure, B] =
    either match {
      case Left(value)  => error(show"Left($value), but expected a Right")
      case Right(value) => pure(value)
    }

  def fromRightT[F[_]: Functor, A: Show, B](
      either: EitherT[F, A, B]
  ): Test[F, B] =
    eval(either.value).flatMap(fromRight(_))

  def fromLeft[A, B: Show](either: Either[A, B]): Test[Pure, A] =
    either match {
      case Right(value) => error(show"Right($value), but expected a Left")
      case Left(value)  => pure(value)
    }

  def fromLeftT[F[_]: Functor, A, B: Show](
      either: EitherT[F, A, B]
  ): Test[F, A] =
    eval(either.value).flatMap(fromLeft(_))

  def fromOption[A](option: Option[A]): Test[Pure, A] =
    option.fold[Test[Pure, A]](error(show"None, but expected a Some"))(pure)

  def fromOptionT[F[_]: Functor, A](option: OptionT[F, A]): Test[F, A] =
    eval(option.value).flatMap(fromOption)

  def pure[A](value: A): Test[Pure, A] = Test.Success(value)

  def label[F[_], A](description: String, test: Test[F, A]): Test[F, A] =
    Test.Label(description, test)

  def not[F[_], A](test: Test[F, A]): Test[F, A] = Test.Not(test)

  def message[F[_], A](description: String, test: Test[F, A]): Test[F, A] =
    Test.Message(description, test)

  def or[F[_], A](tests: List[Test[F, A]]): Test[F, A] = Test.Or(tests)

  def skip[F[_], A](test: Test[F, A]): Test[F, A] = Test.Skip(test)

  val unit: Test[Pure, Unit] = pure(())

  def allOf[F[_], A](tests: Test[F, A]*): Test[F, A] =
    if (tests.isEmpty) empty
    else if (tests.length == 1) tests.head
    else tests.reduceLeft(_ and _)

  def test[F[_], A](
      description: String
  )(test: Test[F, A], tests: Test[F, A]*): Test[F, A] =
    testAll(description)(test +: tests.toList)

  def testAll[F[_], A](
      description: String
  )(tests: List[Test[F, A]]): Test[F, A] =
    label(description, allOf(tests: _*))

  /**
    * Create a `Test` that succeeds when at least one of the given `tests`
    * succeeds
    */
  def oneOf[F[_], A](tests: Test[F, A]*): Test[F, A] =
    if (tests.isEmpty) empty
    else if (tests.length == 1) tests.head
    else tests.reduceLeft(_ or _)

  def success[A](description: String)(value: A): Test[Pure, A] =
    label(description, pure(value))
}
