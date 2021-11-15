package io.taig.testf

enum Result:
  case Success
  case Skipped
  case Error(message: String)
  case Failure(throwable: Throwable)

  def isSuccess: Boolean = this match
    case Success => true
    case _       => false

  def failure: Option[Throwable] = this match
    case Failure(throwable) => Some(throwable)
    case _                  => None
