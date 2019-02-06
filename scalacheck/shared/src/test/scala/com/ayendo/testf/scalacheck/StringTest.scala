package com.ayendo.testf.scalacheck

import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.cond("startsWith", (a + b).startsWith(a))
    }

  val concatenate: Test[Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.label("concatenate",
                 Test.cond("lengthA", (a + b).length >= a.length) |+|
                   Test.cond("lengthB", (a + b).length >= b.length))
    }

  val substring: Test[Unit] =
    Test.check3(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        Test.equal("substring",
                   (a + b + c).substring(a.length, a.length + b.length),
                   b)
    }

  override val suite: IO[Test[Unit]] =
    IO.pure(startsWith |+| concatenate |+| substring)
}
