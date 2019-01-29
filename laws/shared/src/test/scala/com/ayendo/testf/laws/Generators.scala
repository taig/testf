package com.ayendo.testf.laws

import cats._
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit val arbitraryReport: Arbitrary[Report] = {
    val error = (description, description).mapN(Report.Error)

    val failure = (description, summon[Throwable]).mapN(Report.Failure.apply)

    val group = (
      Gen.choose(0, 6).flatMap(Gen.listOfN(_, Gen.lzy(summon[Report]))),
      Gen.option(description)
    ).mapN(Report.Group)

    val skip = description.map(Report.Skip)

    val success = description.map(Report.Success)

    Arbitrary(Gen.oneOf(error, failure, group, skip, success))
  }

  implicit val cogenReport: Cogen[Report] = Cogen.apply({
    case _: Report.Error   => 1
    case _: Report.Failure => 2
    case _: Report.Group   => 3
    case _: Report.Skip    => 4
    case _: Report.Success => 5
  }: Report => Long)

  implicit def arbitraryTestF[F[_]: Applicative, A: Arbitrary]
    : Arbitrary[Test[F, A]] = {
    val genError = (description, summon[String]).mapN(error[F])

    val genFailure = (description, summon[Throwable]).mapN(failure[F])

    val genGroup =
      Gen.lzy((summon[Test[F, A]], summon[Test[F, A]]).mapN(_ |+| _))

    val genLabel = Gen.lzy((description, summon[Test[F, A]]).mapN(label))

    val genPure = (description, summon[A]).mapN(pure[F, A])

    val genSkip = Gen.lzy(summon[Test[F, A]].map(skip))

    val genSuccess = description.map(success[F])

    val genSuspend =
      Gen.lzy((description, summon[A].map(_.pure[F])).mapN(defer[F, A]))

    val generator = Gen.oneOf(genError,
                              genFailure,
                              genGroup,
                              genLabel,
                              genPure,
                              genSkip,
                              genSuccess,
                              genSuspend)

    Arbitrary(generator)
  }

  implicit def arbitraryTestId[A: Arbitrary]: Arbitrary[Test[Id, A]] =
    arbitraryTestF[Id, A]

  implicit def cogenTestF[F[_], A]: Cogen[Test[F, A]] =
    Cogen({
      case Test.Suspend(_)  => 1
      case Test.Error(_)    => 2
      case Test.Failure(_)  => 3
      case Test.Group(_)    => 4
      case Test.Label(_, _) => 5
      case Test.Pure(_)     => 6
      case Test.Skip(_)     => 7
      case Test.Success()   => 8
    }: Test[F, A] => Long)

  implicit def cogenTestId[A: Cogen]: Cogen[Test[Id, A]] = cogenTestF[Id, A]
}
