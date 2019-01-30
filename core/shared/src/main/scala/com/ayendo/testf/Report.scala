package com.ayendo.testf

import cats._
import cats.implicits._
import sbt.testing.Status
import com.ayendo.testf.instance.AllInstances._

sealed trait Report extends Product with Serializable {
  final def success: Boolean =
    PartialFunction.cond(this) {
      case Report.Group(reports)        => reports.forall(_.success)
      case Report.Label(report, _)      => report.success
      case Report.Message(report, _)    => report.success
      case Report.Success               => true
      case Report.Stacktrace(report, _) => report.success
    }

  final def error: Boolean =
    PartialFunction.cond(this) {
      case Report.Error                 => true
      case Report.Group(reports)        => reports.exists(_.error)
      case Report.Label(report, _)      => report.error
      case Report.Message(report, _)    => report.error
      case Report.Stacktrace(report, _) => report.error
    }

  final def failure: Boolean =
    PartialFunction.cond(this) {
      case Report.Failure               => true
      case Report.Group(reports)        => reports.exists(_.failure)
      case Report.Label(report, _)      => report.failure
      case Report.Message(report, _)    => report.failure
      case Report.Stacktrace(report, _) => report.failure
    }

  final def status: Status = this match {
    case Report.Error                 => Status.Error
    case Report.Failure               => Status.Failure
    case Report.Group(reports)        => reports.map(_.status).combineAll
    case Report.Label(report, _)      => report.status
    case Report.Message(report, _)    => report.status
    case Report.Skip                  => Status.Skipped
    case Report.Stacktrace(report, _) => report.status
    case Report.Success               => Status.Success
  }

  final def cause: Option[Throwable] = this match {
    case Report.Error                    => None
    case Report.Failure                  => None
    case Report.Group(reports)           => reports.collectFirstSome(_.cause)
    case Report.Label(report, _)         => report.cause
    case Report.Message(report, _)       => report.cause
    case Report.Skip                     => None
    case Report.Success                  => None
    case Report.Stacktrace(_, throwable) => Some(throwable)
  }
}

object Report {
  case object Error extends Report

  case object Failure extends Report

  case class Group(reports: List[Report]) extends Report

  case class Label(report: Report, value: String) extends Report

  case class Message(report: Report, value: String) extends Report

  case object Skip extends Report

  case class Stacktrace(report: Report, throwable: Throwable) extends Report

  case object Success extends Report

  def error(label: String, message: String): Report =
    Label(Message(Error, message), label)

  def failure(label: String, throwable: Throwable): Report =
    Label(Stacktrace(Failure, throwable), label)

  def group(reports: Report*): Report = Group(reports.toList)

  def label(value: String, report: Report): Report = Label(report, value)

  def message(value: String, report: Report): Report = Message(report, value)

  def skip(description: String): Report = Label(Skip, description)

  def success(description: String): Report = Label(Success, description)

  implicit val semigroup: Semigroup[Report] = {
    case (Group(xs), Group(ys)) => Group(xs ++ ys)
    case (Group(xs), y)         => Group(xs :+ y)
    case (x, Group(ys))         => Group(x +: ys)
    case (x, y)                 => Group(List(x, y))
  }

  implicit val eq: Eq[Report] = new Eq[Report] {
    override def eqv(x: Report, y: Report): Boolean =
      PartialFunction.cond((x, y)) {
        case (Error, Error)                           => true
        case (Failure, Failure)                       => true
        case (Group(xs), Group(ys))                   => xs === ys
        case (Label(rx, mx), Label(ry, my))           => eqv(rx, ry) && mx === my
        case (Message(rx, mx), Message(ry, my))       => eqv(rx, ry) && mx === my
        case (Skip, Skip)                             => true
        case (Stacktrace(rx, tx), Stacktrace(ry, ty)) => eqv(rx, ry) && tx == ty
        case (Success, Success)                       => true
      }
  }

  implicit val show: Show[Report] = {
    case Error                    => "Error"
    case Failure                  => "Failure"
    case Group(reports)           => show"Group($reports)"
    case Label(report, message)   => show"Label($report, $message)"
    case Message(report, message) => show"Message($report, $message)"
    case Skip                     => "Skip"
    case Stacktrace(report, throwable) =>
      show"Stacktrace($report, ${throwable.getMessage})"
    case Success => "Success"
  }
}
