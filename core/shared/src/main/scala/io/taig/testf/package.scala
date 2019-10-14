package io.taig

package object testf {
  type Assertion = Test[Pure, Unit]

  object Assertion extends Assertions {
    def apply(predicate: Boolean, message: => String): Assertion =
      if (predicate) Test.unit else Test.error(message)
  }

  type Pure[A] <: Nothing
}
