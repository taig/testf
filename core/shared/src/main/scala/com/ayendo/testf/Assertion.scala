package com.ayendo.testf

import cats.{Eq, Show}

sealed trait Assertion

object Assertion {
  implicit val eq: Eq[Assertion] = (_, _) => true

  implicit val show: Show[Assertion] = _ => "Assertion"
}
