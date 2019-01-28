package com.ayendo.testf

import cats._
import cats.implicits._
import sbt.testing.Status

sealed trait Report extends Product with Serializable {
  final def isSuccess: Boolean =
    PartialFunction.cond(this) {
      case Report.Success(_)          => true
      case Report.Group(reports, _) => reports.forall(_.isSuccess)
    }

  final def isErrorOrFailure: Boolean =
    PartialFunction.cond(this) {
      case Report.Error(_, _) | Report.Failure(_, _) => true
      case Report.Group(reports, _)                 => reports.exists(_.isErrorOrFailure)
    }

  def toStatus: Status
}

object Report {
  case class Error(description: String, message: String) extends Report {
    override val toStatus = Status.Error
  }

  case class Failure(description: String, throwable: Throwable)
      extends Report {
    override val toStatus = Status.Failure
  }

  case class Group(reports: List[Report], description: Option[String])
      extends Report {
    override val toStatus = reports.map(_.toStatus).foldLeft(Status.Success) {
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

  case class Skip(description: String) extends Report {
    override val toStatus = Status.Skipped
  }

  case class Success(description: String) extends Report {
    override val toStatus = Status.Success
  }

  implicit val eq: Eq[Report] = new Eq[Report] {
    override def eqv(x: Report, y: Report): Boolean =
      PartialFunction.cond((x, y)) {
        case (Error(dx, mx), Error(dy, my))     => dx === dy && mx === my
        case (Failure(dx, tx), Failure(dy, ty)) => dx === dy && tx == ty
        case (Group(sx, dx), Group(sy, dy))     => sx === sy && dx === dy
        case (Skip(x), Skip(y))                 => x === y
        case (Success(x), Success(y))           => x === y
      }
  }
}
