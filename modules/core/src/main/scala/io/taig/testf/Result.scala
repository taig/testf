package io.taig.testf

enum Result:
  case Success
  case Skipped
  case Error(message: String)
  case Failure(throwable: Throwable)

  def &&(result: Result): Result = (this, result) match
    case (Success, result)    => result
    case (Skipped, result)    => result
    case (result, Skipped)    => result
    case (result: Error, _)   => result
    case (result: Failure, _) => result

  def isSuccess: Boolean = this match
    case Success => true
    case _       => false

  def error: Option[String] = this match
    case Error(message) => Some(message)
    case _              => None

  def failure: Option[Throwable] = this match
    case Failure(throwable) => Some(throwable)
    case _                  => None
