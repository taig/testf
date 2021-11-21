package io.taig.testf

object Assertion:
  def contains(obtained: String, needle: CharSequence): Result =
    if (obtained.contains(needle)) Result.Success else Result.Error(s"$obtained does not contain '$needle'")

  def equals[A](obtained: A, expected: A): Result =
    if (obtained == expected) Result.Success else Result.Error(s"$obtained != $expected")
