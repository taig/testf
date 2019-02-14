package com.ayendo.testf.scalacheck

import cats.Id
import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Id, Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.cond("startsWith", (a + b).startsWith(a))
    }

  val concatenate: Test[Id, Unit] =
    Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
      Test.label("concatenate",
                 Test.cond[Id]("lengthA", (a + b).length >= a.length) |+|
                   Test.cond[Id]("lengthB", (a + b).length >= b.length))
    }

  val substring: Test[Id, Unit] =
    Test.check3(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b, c) =>
        Test.equal("substring",
                   (a + b + c).substring(a.length, a.length + b.length),
                   b)
    }

  override val suite: Test[IO, Unit] =
    (startsWith |+| concatenate |+| substring).mapK(Test.liftId)
}
