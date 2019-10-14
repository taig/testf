package io.taig

package object testf {
  type Assertion = Test[Pure, Unit]

  object Assertion extends Assertions

  type Pure[A] <: Nothing
}
