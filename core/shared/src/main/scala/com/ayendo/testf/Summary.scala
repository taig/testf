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
  case class Error(description: String, message: String) extends Summary {
    override val toStatus = Status.Error
  }

  case class Failure(description: String, throwable: Throwable)
      extends Summary {
    override val toStatus = Status.Failure
  }

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

  case class Skip(description: String) extends Summary {
    override val toStatus = Status.Skipped
  }

  case class Success(description: String) extends Summary {
    override val toStatus = Status.Success
  }

  implicit val eq: Eq[Summary] = new Eq[Summary] {
    override def eqv(x: Summary, y: Summary): Boolean =
      PartialFunction.cond((x, y)) {
        case (Error(dx, mx), Error(dy, my))     => dx === dy && mx === my
        case (Failure(dx, tx), Failure(dy, ty)) => dx === dy && tx == ty
        case (Group(sx, dx), Group(sy, dy))     => sx === sy && dx === dy
        case (Skip(x), Skip(y))                 => x === y
        case (Success(x), Success(y))           => x === y
      }
  }
}
