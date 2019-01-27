package com.ayendo

package object testf extends TestBuilders {
  type Assert[F[_]] = Test[F, Assertion]
}
