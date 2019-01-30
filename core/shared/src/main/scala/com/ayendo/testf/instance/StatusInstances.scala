package com.ayendo.testf.instance

import cats.Monoid
import sbt.testing.Status

trait StatusInstances {
  implicit val monoidStatus: Monoid[Status] = new Monoid[Status] {
    override def empty: Status = Status.Success

    override def combine(x: Status, y: Status): Status = (x, y) match {
      case (Status.Canceled, status)                 => status
      case (status, Status.Canceled)                 => status
      case (_, Status.Error) | (Status.Error, _)     => Status.Error
      case (_, Status.Failure) | (Status.Failure, _) => Status.Failure
      case (Status.Ignored, status)                  => status
      case (status, Status.Ignored)                  => status
      case (Status.Pending, status)                  => status
      case (status, Status.Pending)                  => status
      case (Status.Skipped, status)                  => status
      case (status, Status.Skipped)                  => status
      case (Status.Success, Status.Success)          => Status.Success
      case _                                         => ???
    }
  }
}

object StatusInstances extends StatusInstances
