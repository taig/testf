package com.ayendo.testf.laws

import cats.implicits._
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.MonadTests
import com.ayendo.testf._
import org.scalacheck.Arbitrary

object TestTest extends TestF {
  implicit def arbitrary[A: Arbitrary]: Arbitrary[Test[A]] = {
    val generator = Arbitrary.arbitrary[A].map { value =>
      Test.pure(value.toString, value)
    }

    Arbitrary(generator)
  }

  override val suite: List[Assert] = List(
    Test.verify("SemigroupLaws", SemigroupTests[Test[Int]].semigroup),
    Test.verify("MonadLaws", MonadTests[Test].monad[Int, Int, String])
  )
}
