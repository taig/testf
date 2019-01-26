package com.ayendo.testf.laws

import cats.effect.IO
import cats.implicits._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.MonadTests
import cats.{Applicative, Id}
import com.ayendo.testf._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}

object TestTest extends TestF {
  def summon[A: Arbitrary]: Gen[A] = Arbitrary.arbitrary[A]

  val description: Gen[String] = Gen.choose(4, 16).flatMap { length =>
    Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  }

  implicit def arbitrary[F[_]: Applicative, A: Arbitrary]
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

  implicit def arbitraryId[A: Arbitrary]: Arbitrary[Test[Id, A]] =
    arbitrary[Id, A]

  implicit def cogen[F[_], A: Cogen]: Cogen[Test[F, A]] =
    Cogen { (seed, test) =>
      test match {
        case Test.Defer(_)       => ???
        case Test.Error(_)       => seed
        case Test.Failure(_)     => seed
        case Test.Group(_)       => ???
        case Test.Label(_, test) => cogen[F, A].perturb(seed, test)
        case Test.Pure(value)    => Cogen[A].perturb(seed.next, value)
        case Test.Skip(_)        => ???
        case Test.Success()      => seed
      }
    }

  implicit def cogenId[A: Cogen]: Cogen[Test[Id, A]] = cogen[Id, A]

  override val suite: List[Assert[IO]] = List(
    Test.verify("EqLaws", EqTests[Test[Id, Int]].eqv),
    Test.verify("SemigroupLaws", SemigroupTests[Test[Id, Int]].semigroup),
    Test.verify("MonadLaws",
                MonadTests[Test[Id, ?]].stackUnsafeMonad[Int, Int, String])
  )
}
