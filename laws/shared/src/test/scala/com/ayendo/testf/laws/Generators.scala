package com.ayendo.testf.laws

import cats._
import cats.implicits._
import com.ayendo.testf.{Summary, Test}
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit def arbitraryTestF[F[_]: Applicative, A: Arbitrary]
    : Arbitrary[Test[F, A]] = {
    val defer =
      Gen.lzy((description, summon[A].map(_.pure[F])).mapN(Test.defer[F, A]))

    val error = (description, summon[String]).mapN(Test.error[F, A])

    val failure = (description, summon[Throwable]).mapN(Test.failure[F, A])

    val group = Gen.lzy((summon[Test[F, A]], summon[Test[F, A]]).mapN(_ |+| _))

    val label = Gen.lzy((description, summon[Test[F, A]]).mapN(Test.label))

    val pure = (description, summon[A]).mapN(Test.pure[F, A])

    val skip = Gen.lzy(summon[Test[F, A]].map(Test.skip))

    val success = description.map(Test.success[F, A])

    val generator =
      Gen.oneOf(defer, error, failure, group, label, pure, skip, success)

    Arbitrary(generator)
  }

  implicit def arbitraryTestId[A: Arbitrary]: Arbitrary[Test[Id, A]] =
    arbitraryTestF[Id, A]

  implicit def cogenTestF[F[_], A]: Cogen[Test[F, A]] =
    Cogen({
      case Test.Defer(_)    => 1
      case Test.Error(_)    => 2
      case Test.Failure(_)  => 3
      case Test.Group(_)    => 4
      case Test.Label(_, _) => 5
      case Test.Pure(_)     => 6
      case Test.Skip(_)     => 7
      case Test.Success()   => 8
    }: Test[F, A] => Long)

  implicit def cogenTestId[A: Cogen]: Cogen[Test[Id, A]] = cogenTestF[Id, A]

  implicit val arbitrarySummary: Arbitrary[Summary] = {
    val error = (description, description).mapN(Summary.Error)

    val failure = (description, summon[Throwable]).mapN(Summary.Failure.apply)

    val group = (
      Gen.choose(0, 6).flatMap(Gen.listOfN(_, Gen.lzy(summon[Summary]))),
      Gen.option(description)
    ).mapN(Summary.Group)

    val skip = description.map(Summary.Skip)

    val success = description.map(Summary.Success)

    Arbitrary(Gen.oneOf(error, failure, group, skip, success))
  }

  implicit val cogenSummary: Cogen[Summary] = Cogen.apply({
    case _: Summary.Error   => 1
    case _: Summary.Failure => 2
    case _: Summary.Group   => 3
    case _: Summary.Skip    => 4
    case _: Summary.Success => 5
  }: Summary => Long)
}
