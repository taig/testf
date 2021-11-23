package io.taig.testf

import cats.arrow.FunctionK
import cats.effect.{Concurrent, Temporal}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.{ApplicativeThrow, Parallel, ~>}

import scala.concurrent.duration.*

final class ConcurrentRunner[F[_]: ApplicativeThrow: Parallel: Temporal, G[_]](gK: G ~> F) extends Runner[F, G]:
  override def run(spec: Spec[G]): F[Report] =
    val evaluation = spec.test match
      case Test.Pure(result) => Evaluation.Yield(result()).pure[F]
      case Test.Effect(result) =>
        gK(result).map(Evaluation.Yield.apply).handleError(throwable => Evaluation.Yield(Result.Failure(throwable)))
      case Test.Group(specs, false) => specs.traverse(run).map(Evaluation.Group.apply)
      case Test.Group(specs, true)  => specs.parTraverse(run).map(Evaluation.Group.apply)

    spec.configuration.timeout match
      case duration: FiniteDuration => evaluation.timeout(duration).map(Report(spec.name, _))
      case _: Duration.Infinite => evaluation.map(Report(spec.name, _))

object ConcurrentRunner:
  def apply[F[_]: ApplicativeThrow: Parallel: Temporal, G[_]](gK: G ~> F): Runner[F, G] = new ConcurrentRunner[F, G](gK)

  def apply[F[_]: ApplicativeThrow: Parallel: Temporal]: Runner[F, F] = new ConcurrentRunner[F, F](FunctionK.id)
