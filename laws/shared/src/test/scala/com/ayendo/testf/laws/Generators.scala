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
    val error = Gen.const(Report.Error)

    val failure = Gen.const(Report.Failure)

    val group = for {
      length <- Gen.choose(0, 6)
      reports <- Gen.listOfN(length, Gen.lzy(summon[Report]))
    } yield Report.Group(reports)

    val label = (Gen.lzy(summon[Report]), description).mapN(Report.Label)

    val message = (Gen.lzy(summon[Report]), description).mapN(Report.Message)

    val skip = Gen.const(Report.Skip)

    val stacktrace =
      (Gen.lzy(summon[Report]), summon[Throwable]).mapN(Report.Stacktrace)

    val success = Gen.const(Report.Success)

    Arbitrary(Gen
      .oneOf(error, failure, group, label, message, skip, stacktrace, success))
  }

  implicit val cogenReport: Cogen[Report] = Cogen.apply({
    case Report.Error            => 1
    case Report.Failure          => 2
    case Report.Group(_)         => 3
    case Report.Label(_, _)      => 4
    case Report.Message(_, _)    => 5
    case Report.Skip             => 6
    case Report.Stacktrace(_, _) => 7
    case Report.Success          => 8
  }: Report => Long)

  implicit def arbitraryTestF[F[_]: Applicative, A: Arbitrary]
    : Arbitrary[Test[F, A]] = {
    val error = (description, summon[String]).mapN(Test.error[F])

    val failure = (description, summon[Throwable]).mapN(Test.failure[F])

    val group =
      Gen.lzy((summon[Test[F, A]], summon[Test[F, A]]).mapN(_ |+| _))

    val label = Gen.lzy((description, summon[Test[F, A]]).mapN(Test.label))

    val report = summon[Report].map(Test.Result[F])

    val skip = Gen.lzy(summon[Test[F, A]].map(Test.skip))

    val success = (description, summon[A]).mapN(Test.success[F, A])

    val suspend =
      Gen.lzy((description, summon[A].map(_.pure[F])).mapN(Test.defer[F, A]))

    val generator =
      Gen.oneOf(error, failure, group, label, report, skip, success, suspend)

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
      case Test.Result(_)   => 6
      case Test.Skip(_)     => 7
      case Test.Success(_)  => 8
    }: Test[F, A] => Long)

  implicit def cogenTestId[A: Cogen]: Cogen[Test[Id, A]] = cogenTestF[Id, A]
}
