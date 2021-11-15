package io.taig.testf

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream

final class ConcurrentRunner[F[_]: Concurrent] extends Runner[F, F]:
  override def run(test: Test[F]): F[Report] = test match
    case Test.Assertion(result) => Report.Assertion(result()).pure[F]
    case Test.AssertionF(result) =>
      result.map(Report.Assertion.apply).handleError(throwable => Report.Assertion(Result.Failure(throwable)))
    case Test.Group(tests, 1) => tests.traverse(run).map(Report.Group.apply)
    case Test.Group(tests, concurrency) =>
      Stream.emits(tests).parEvalMap(concurrency)(run).compile.toList.map(Report.Group.apply)
    case Test.Label(name, test) => run(test).map(Report.Label(name, _))
    case Test.Skip(test)        => Runner.skipAll(test).pure[F]

object ConcurrentRunner:
  def apply[F[_]: Concurrent]: Runner[F, F] = new ConcurrentRunner[F]
