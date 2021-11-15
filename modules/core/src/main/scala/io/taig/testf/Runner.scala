package io.taig.testf

abstract class Runner[F[_], G[_]]:
  def run(test: Test[G]): F[Report]

object Runner:
  def skipAll(test: Test[_]): Report = test match
    case Test.Assertion(_)      => Report.Assertion(Result.Skipped)
    case Test.AssertionF(_)     => Report.Assertion(Result.Skipped)
    case Test.Group(tests, _)   => Report.Group(tests.map(skipAll))
    case Test.Label(name, test) => Report.Label(name, skipAll(test))
    case Test.Skip(test)        => skipAll(test)

  val pure: Runner[Identity, Pure] = new Runner[Identity, Pure]:
    override def run(test: Test[Pure]): Report = test match
      case Test.Assertion(result)  => Report.Assertion(result())
      case Test.AssertionF(result) => Report.Assertion(result)
      case Test.Group(tests, _)    => Report.Group(tests.map(run))
      case Test.Label(name, test)  => Report.Label(name, run(test))
      case Test.Skip(test)         => skipAll(test)
