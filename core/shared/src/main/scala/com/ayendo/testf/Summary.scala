package com.ayendo.testf

import cats._
import cats.implicits._
import sbt.testing.Status

sealed trait Summary extends Product with Serializable {
  final def isSuccess: Boolean =
    PartialFunction.cond(this) {
      case Summary.Success(_)          => true
      case Summary.Group(summaries, _) => summaries.forall(_.isSuccess)
    }

  final def isErrorOrFailure: Boolean =
    PartialFunction.cond(this) {
      case Summary.Error(_, _) | Summary.Failure(_, _) => true
      case Summary.Group(summaries, _)                 => summaries.exists(_.isErrorOrFailure)
    }

  def toStatus: Status
}

object Summary {
  case class Group(summaries: List[Summary], description: Option[String])
      extends Summary {
    override val toStatus = summaries.map(_.toStatus).foldLeft(Status.Success) {
      case (Status.Error, _)                => Status.Error
      case (_, Status.Error)                => Status.Error
      case (Status.Failure, _)              => Status.Failure
      case (_, Status.Failure)              => Status.Failure
      case (Status.Success, Status.Success) => Status.Success
      case (Status.Skipped, status)         => status
      case (status, Status.Skipped)         => status
      case _                                => ???
    }
  }

  sealed trait Row extends Summary {
    def description: String
  }

  case class Error(description: String, message: String) extends Row {
    override val toStatus = Status.Error
  }

  case class Failure(description: String, throwable: Throwable) extends Row {
    override val toStatus = Status.Failure
  }

  object Failure {
    implicit val eq: Eq[Failure] = (x, y) =>
      x.description === y.description && x.throwable == y.throwable
  }

  case class Skip(description: String) extends Row {
    override val toStatus = Status.Skipped
  }

  case class Success(description: String) extends Row {
    override val toStatus = Status.Success
  }

  implicit val eq: Eq[Summary] = derived.semi.eq[Summary]
}
