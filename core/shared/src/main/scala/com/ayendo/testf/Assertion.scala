package com.ayendo.testf

import cats.{Eq, Show}

sealed trait Assertion

object Assertion extends Assertion {
  implicit val eq: Eq[Assertion] = Eq.allEqual

  implicit val show: Show[Assertion] = _ => "Assertion"
}
