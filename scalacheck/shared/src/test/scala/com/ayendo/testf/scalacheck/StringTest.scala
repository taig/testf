package com.ayendo.testf.scalacheck

import cats.effect.IO
import cats.implicits._
import com.ayendo.testf._
import org.scalacheck.Gen

object StringTest extends TestF {
  val startsWith: Test[Pure] =
    Test.label("startsWith", Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) {
      (a, b) =>
        Test.startsWith(a + b, a)
    })

  val concatenate: Test[Pure] =
    Test.label(
      "concatenate",
      Test.check2(Gen.alphaNumStr, Gen.alphaNumStr) { (a, b) =>
        Test.gte((a + b).length, a.length) ~ "lengthA" |+|
          Test.gte((a + b).length, b.length) ~ "lengthB"
      }
    )

  val substring: Test[Pure] =
    Test.label(
      "substring",
      Test.check3(
        Gen.alphaNumStr,
        Gen.alphaNumStr,
        Gen.alphaNumStr
      ) { (a, b, c) =>
        Test.equal((a + b + c).substring(a.length, a.length + b.length), b)
      }
    )

  override val suite: IO[Test[Pure]] =
    Test.of(startsWith, concatenate, substring).compile
}
