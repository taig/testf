package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Assert[Id] =
    check2("startsWith")(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b) startsWith a
    }

  val concatenate: Assert[Id] =
    check2("concatenate")(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      (a + b).length >= a.length && (a + b).length >= b.length
    }

  val substring: Assert[Id] =
    check3("substring")(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
    }

  override val suite: Assert[IO] =
    (startsWith |+| concatenate |+| substring).liftIO
}
