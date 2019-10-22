package io.taig.testf.internal

import cats._
import cats.implicits._
import io.taig.testf.Test

object Tests {
  val count: Test[Id, _] => Int = {
    case Test.And(tests)       => tests.length
    case Test.Eval(test)       => count(test)
    case Test.Error(_)         => 1
    case Test.Failure(_)       => 1
    case Test.Label(_, test)   => count(test)
    case Test.Message(_, test) => count(test)
    case Test.Not(test)        => count(test)
    case Test.Or(tests)        => tests.length
    case Test.Skip(_)          => 0
    case Test.Success(_)       => 1
  }

  val label: Test[Id, _] => Option[String] = {
    case Test.And(_)                => none
    case Test.Eval(test)            => label(test)
    case Test.Error(_)              => none
    case Test.Failure(_)            => none
    case Test.Label(description, _) => description.some
    case Test.Message(_, test)      => label(test)
    case Test.Not(test)             => label(test)
    case Test.Or(_)                 => none
    case Test.Skip(test)            => label(test)
    case Test.Success(_)            => none
  }

  def children[A]: Test[Id, A] => List[Test[Id, A]] = {
    case Test.And(tests)       => tests
    case Test.Eval(test)       => children(test)
    case test: Test.Error      => List(test: Test[Id, A])
    case test: Test.Failure    => List(test: Test[Id, A])
    case Test.Label(_, test)   => children(test)
    case Test.Message(_, test) => children(test)
    case Test.Not(test)        => children(test)
    case Test.Or(tests)        => tests
    case Test.Skip(_)          => List.empty[Test[Id, A]]
    case test: Test.Success[A] => List(test: Test[Id, A])
  }

  val throwable: Test[Id, _] => Option[Throwable] = {
    case Test.And(tests)         => tests.collectFirstSome(throwable)
    case Test.Eval(test)         => throwable(test)
    case Test.Error(_)           => none
    case Test.Failure(throwable) => throwable.some
    case Test.Label(_, test)     => throwable(test)
    case Test.Message(_, test)   => throwable(test)
    case Test.Not(test)          => throwable(test)
    case Test.Or(tests)          => tests.collectFirstSome(throwable)
    case Test.Skip(_)            => none
    case Test.Success(_)         => none
  }
}
