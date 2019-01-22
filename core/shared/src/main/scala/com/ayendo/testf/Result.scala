package com.ayendo.testf

import cats._
import cats.implicits._

sealed trait Result extends Product with Serializable

object Result {
  case object Success extends Result

  case class Error(message: String) extends Result

  implicit val eq: Eq[Result] = derived.semi.eq[Result]
}
