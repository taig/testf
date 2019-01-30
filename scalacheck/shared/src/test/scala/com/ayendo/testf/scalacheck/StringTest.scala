package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Id, Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.pure("startsWith", a + b).startsWith(a)
    }

  val concatenate: Test[Id, Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.label(
        "concatenate",
        Test.pure("lengthA", (a + b).length >= a.length).isTrue |+| Test
          .pure("lengthB", (a + b).length >= b.length)
          .isTrue)
    }

  val substring: Test[Id, Unit] =
    Test.check3(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        Test
          .pure("substring",
                (a + b + c).substring(a.length, a.length + b.length))
          .equal(b)
    }

  override val suite: Test[IO, Unit] =
    (startsWith |+| concatenate |+| substring).liftIO
}
