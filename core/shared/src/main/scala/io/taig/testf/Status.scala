package io.taig.testf

sealed abstract class Status extends Product with Serializable

object Status {
  final case object Error extends Status
  final case object Failure extends Status
  final case object Skip extends Status
  final case object Success extends Status

  private val and: (Status, Status) => Status = {
    case (x, Skip)    => x
    case (Skip, y)    => y
    case (_, Failure) => Failure
    case (Failure, _) => Failure
    case (_, Error)   => Error
    case (Error, _)   => Error
    case (x, Success) => x
    case (Success, y) => y
  }

  private val or: (Status, Status) => Status = {
    case (x, Skip)    => x
    case (Skip, y)    => y
    case (x, Failure) => x
    case (Failure, y) => y
    case (x, Error)   => x
    case (Error, y)   => y
    case (x, Success) => x
    case (Success, y) => y
  }

  private val not: Status => Status = {
    case Success => Error
    case Error   => Success
    case status  => status
  }

  def of(test: Test[Pure]): Status = test match {
    case Test.And(tests)       => tests.map(of).foldLeft[Status](Success)(and)
    case test: Test.Eval[Pure] => of(test.test)
    case Test.Error(_)         => Status.Error
    case Test.Failure(_)       => Status.Failure
    case Test.Label(_, test)   => of(test)
    case Test.Message(_, test) => of(test)
    case Test.Not(test)        => not(of(test))
    case Test.Or(tests)        => tests.map(of).foldLeft[Status](Success)(or)
    case Test.Skip(_)          => Status.Skip
    case Test.Success          => Status.Success
  }
}
