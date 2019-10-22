package io.taig.testf

import org.scalacheck.Cogen
import org.scalacheck.rng.Seed

object Cogens {
  implicit val cogenTest: Cogen[Assertion[Pure]] = Cogen({
    case (seed, Test.And(tests))             => Cogen.perturb(seed, tests)
    case (seed, test: Test.Eval[Pure, Unit]) => Cogen.perturb(seed, test.test)
    case (seed, Test.Error(message))         => Cogen.perturb(seed, message)
    case (seed, Test.Failure(throwable))     => Cogen.perturb(seed, throwable)
    case (seed, Test.Label(description, test)) =>
      Cogen.perturb(seed, (description, test))
    case (seed, Test.Message(description, test)) =>
      Cogen.perturb(seed, (description, test))
    case (seed, Test.Not(test))  => Cogen.perturb(seed, test)
    case (seed, Test.Or(tests))  => Cogen.perturb(seed, tests)
    case (seed, Test.Skip(test)) => Cogen.perturb(seed, test)
    case (seed, Test.Success(_)) => seed
  }: (Seed, Assertion[Pure]) => Seed)
}
