package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Id, Assertion] =
    check2("startsWith")(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b) startsWith a
    }

  val concatenate: Test[Id, Assertion] =
    check2("concatenate")(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b).length >= a.length && (a + b).length >= b.length
    }

  val substring: Test[Id, Assertion] =
    check3("substring")(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    }

  override val suite: Test[IO, Assertion] =
    (startsWith |+| concatenate |+| substring).liftIO
}
