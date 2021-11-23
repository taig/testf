package io.taig.testf

abstract class Runner[F[_], G[_]]:
  def run(spec: Spec[G]): F[Report]

object Runner:
  val skipAll: Runner[Identity, Pure] = new Runner[Identity, Pure]:
    override def run(spec: Spec[Pure]): Report =
      val result = spec.test match
        case Test.Pure(_) | Test.Effect(_) => Evaluation.Yield(Result.Skipped)
        case Test.Group(specs, _)          => Evaluation.Group(specs.map(run))

      Report(spec.name, result)

  val pure: Runner[Identity, Pure] = new Runner[Identity, Pure]:
    override def run(spec: Spec[Pure]): Report =
      val evaluation = spec.test match
        case Test.Effect(result)  => Evaluation.Yield(result)
        case Test.Pure(result)    => Evaluation.Yield(result())
        case Test.Group(specs, _) => Evaluation.Group(specs.map(run))

      Report(spec.name, evaluation)
