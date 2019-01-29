package com.ayendo.testf

import cats._
import cats.implicits._
import sbt.testing.Status

sealed trait Report extends Product with Serializable {
  final def success: Boolean =
    PartialFunction.cond(this) {
      case Report.Success(_)        => true
      case Report.Group(reports, _) => reports.forall(_.success)
    }

  final def errorOrFailure: Boolean =
    PartialFunction.cond(this) {
      case Report.Error(_, _) | Report.Failure(_, _) => true
      case Report.Group(reports, _)                  => reports.exists(_.errorOrFailure)
    }

  def status: Status
}

object Report {
  case class Error(description: String, message: String) extends Report {
    override val status = Status.Error
  }

  case class Failure(description: String, throwable: Throwable) extends Report {
    override val status = Status.Failure
  }

  case class Group(reports: List[Report], description: Option[String])
      extends Report {
    override val status = reports.map(_.status).foldLeft(Status.Success) {
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
    override val status = Status.Skipped
  }

  case class Success(description: String) extends Report {
    override val status = Status.Success
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
