package io.taig

import cats._

package object testf {
  type Assertion[F[_]] = Test[F, Unit]

  object Assertion extends Assertions {
    def apply(predicate: Boolean, message: => String): Assertion[Pure] =
      if (predicate) Test.unit else Test.error(message)
  }

  type Pure[A] <: Nothing

  implicit val functor: Functor[Pure] = Functor[Id].asInstanceOf[Functor[Pure]]
}
