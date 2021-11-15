package io.taig.testf

object Assertion:
//  def eq[A: Eq](obtained: A, expected: A): Result =
//    if (obtained === expected) Result.Success else Result.Error(s"$obtained =!= $expected")

  def equals[A](obtained: A, expected: A): Result =
    if (obtained == expected) Result.Success else Result.Error(s"$obtained != $expected")
