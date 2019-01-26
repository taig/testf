package com.ayendo

package object testf {
  type Assert[F[_]] = Test[F, Assertion]
}
